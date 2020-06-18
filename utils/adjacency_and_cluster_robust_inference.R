# Import libraries
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(lfe)
library(jsonlite)
library(ggplot2)
library(onehot)
library(reshape2)
library(sandwich)
library(lmtest)
library(foreach)

## The code below contains helper functions that are needed in the felm version of waldtest. These 
## are not modified, and copied straight from the innards of felm.

pinvx <- function(X) {
#    return(pinv(nazero(X)))
    ch <- cholx(nazero(X))
    badvars <- attr(ch,'badvars')
    inv1 <- chol2inv(ch)
    if(is.null(badvars)) return(inv1)
    inv <- matrix(0,nrow(X),ncol(X))
    inv[-badvars,-badvars] <- inv1
    structure(inv,badvars=attr(ch,'badvars'))
}

cholx <- function(mat, eps=1e-6) {
  if(is.null(dim(mat))) dim(mat) <- c(1,1)
  N <- dim(mat)[1]
  if(N == 1) {
      return(structure(sqrt(mat),badvars=if(mat<=0) 1 else NULL))
  }

  # first, try a pivoted one
  tol <- N*getOption('lfe.eps')
  chp <- chol(mat,pivot=TRUE,tol=tol)
  rank <- attr(chp,'rank')
  if(rank == N) return(chol(mat))
  pivot <- attr(chp,'pivot')
  oo <- order(pivot)
  badvars <- pivot[((rank+1):N)]
  ok <- (1:N)[-badvars]
  ch <- chol(mat[ok,ok])
  return(structure(ch,badvars=badvars))
}
                       
nazero <- function(x) ifelse(is.na(x),0,x)

## Below is a modified version of the felm waldtest method. Rather than using one of felm's vcov matrices 
## depending on the "type" argument, this function takes an arbitrary covariance matrix as an argument, 
# and then uses that.

# Note that when using for our analysis, you need to specify df2 = n_clusters.

felm.waldtest.mod <- function(object, vcov, R, r, lhs=NULL, df1, df2) {
  if(inherits(object,'felm') && object$nostats) stop('No Wald test for objects created with felm(nostats=TRUE)')
  type <- "cluster"
  # We make a chi^2 to test whether the equation R theta = r holds.
  # The chi^2 is computed according to Wooldridge (5.34, 10.59).
  # I.e. a Wald test W = N*(beta' (R V^{-1} R')^{-1} beta) where beta = R theta - r
  # W is chi2 with length(r) df,
  # and V is th covariance matrix.

  # First, find V. It's in either object$vcv, object$robustvcv or object$clustervcv
  if(is.null(lhs) && length(object$lhs) > 1) {
    stop('Please specify lhs=[one of ',paste(object$lhs, collapse=','),']')
  }
  if(!is.null(lhs) && is.na(match(lhs, object$lhs)))
      stop('Please specify lhs=[one of ',paste(object$lhs, collapse=','),']')

  V <- vcov

#  if(is.null(lhs) && length(object$lhs) == 1) lhs <- object$lhs
  cf <- coef(object)
  if(is.matrix(cf))
      nmc <- rownames(cf)
  else
      nmc <- names(cf)

  if(inherits(R,'formula') || is.call(R) || is.name(R)) {
    Rr <- formtoR(R, nmc)
    R <- Rr[,-ncol(Rr), drop=FALSE]
    r <- Rr[,ncol(Rr)]
  } else if(is.function(R)) {
    # non-linear stuff. Compute value and gradient of R
  if(!requireNamespace('numDeriv', quietly=TRUE)) {warning("package numDeriv must be available to use non-linear Wald test"); return(NULL)}
    pt <- coef(object,lhs=lhs)
    pt[is.na(pt)] <- 0
    val <- R(pt)
    if(is.null(dim(val))) dim(val) <- c(length(val), 1)
    gr <- numDeriv::jacobian(R,pt)
    if(is.null(dim(gr))) dim(gr) <- c(1,length(gr))
  } else  if(!is.matrix(R)) {
    # it's not a matrix, so it's a list of parameters, either
    # names, logicals or indices
    if(is.null(R)) R <- nmc
    if(is.character(R)) {
      ev <- match('endovars', R)
      if(!is.na(ev)) {
        # replace with endogenous variables
        R <- c(R[-ev],object$endovars)
      }
      # did user specify any of the endogenous variables?
      fitvars <- paste('`',R,'(fit)`',sep='')
      fitpos <- match(fitvars,nmc)
      # replace those which are not NA
      noNA <- which(!is.na(fitpos))
      R[noNA] <- fitvars[noNA]
      Ri <- match(R, nmc)
      if(anyNA(Ri)) stop("Couldn't find variables ",paste(R[is.na(Ri)],collapse=','))
      R <- Ri
    } else if(is.logical(R)) {
      R <- which(R)
    }
    # here R is a list of positions of coefficients
    # make the projection matrix.

    RR <- matrix(0,length(R),length(coef(object,lhs=lhs)))
    for(i in seq_along(R)) {
      RR[i,R[i]] <- 1
    }
    R <- RR
  } 
  # Two cases here. If R is a function, we do a non-linear delta test against 0, otherwise
  # we do the ordinary Wald test
  if(is.function(R)) {
    W <- as.numeric(t(val) %*% solve(gr %*% V %*% t(gr)) %*% val)
    if(missing(df1)) df1 <- length(val)
  } else {
    if(missing(r) || is.null(r))
        r <- rep(0,nrow(R))
    else if(length(r) != nrow(R)) stop('nrow(R) != length(r)')
    cf <- coef(object, lhs=lhs)
    cf[is.na(cf)] <- 0
    beta <- R %*% cf - r
    V[is.na(V)] <- 0   # ignore NAs
    se <- try(1/sqrt(solve(R %*% V %*% t(R))), silent=TRUE)
    W <- try(sum(beta * solve(R %*% V %*% t(R),beta)), silent=TRUE)
    
    if(inherits(W,'try-error')) 
        W <- as.numeric(t(beta) %*% pinvx(R %*% V %*% t(R)) %*% beta)
  }

  # W follows a chi2(Q) distribution, but the F-test has another
  # df which is ordinarily object$df. However, if there are clusters
  # the df should be reduced to the number of clusters-1
  if(missing(df2)) {
    df2 <- object$df
    if((!is.null(object$clustervar) && type %in% c('default','cluster')) ) {
      df2 <- min(nlevels(object$clustervar[[1]])-1, df2)
    }
  }

  if(missing(df1))
      df1 <- length(beta)

  F <- W/df1
  # F follows a F(df1,df2) distribution
  if(is.function(R)) frm <- R else  frm <- Rtoform(R,r,nmc)
  
  structure(c(beta=beta, se=se, p=pchisq(W, df1, lower.tail=FALSE), chi2=W, df1=df1,
              p.F=pf(F,df1,df2, lower.tail=FALSE), F=F, df2=df2),
            formula=frm)
}

# convert a formula which is a set of linear combinations like ~x+x3 | x2-x4+3 to
# matrices R and r such that R %*% coefs = r
# the vector r is return as the last column of the result
formtoR <- function(formula, coefs) {

  conv <- function(f) formtoR(f, coefs)

  lf <- as.list(formula)
  if(lf[[1]] == as.name('~') || lf[[1]] == as.name('quote')) return(conv(lf[[2]]))
  # here we have a single formula w/o '~' in front, e.g. x+x3|x2-x4, or just x+x3
  # split off parts '|' in a loop
  R <- NULL
#  if(length(lf) != 1) stop('length of ',lf, ' is != 1')
#  lf <- as.list(lf[[1]])
  op <- lf[[1]]
  if(op == as.name('|')) {
    return(rbind(conv(lf[[2]]), conv(lf[[3]])))
  } else if(op == as.name('+')) {
    if(length(lf) == 2) return(conv(lf[[2]])) # unary +
    return(conv(lf[[2]]) + conv(lf[[3]]))
  } else if(op == as.name('-')) {
    if(length(lf) == 2) return(-conv(lf[[2]])) # unary -
    return(conv(lf[[2]]) - conv(lf[[3]]))    
  } else if(op == as.name('*')) {
    f1 <- conv(lf[[2]])
    f2 <- conv(lf[[3]])
    # the first one must be a numeric, i.e. only last column filled in
    # and it's negative
    fac <- -f1[length(f1)]
    return(fac * conv(lf[[3]])) 
  } else if(is.name(op)) {
    res <- matrix(0,1,length(coefs)+1)
    pos <- match(as.character(op), coefs)
    if(is.na(pos)) {
      ivspec <- paste("`",as.character(op),"(fit)`", sep='')
      pos <- match(ivspec, coefs)
    }
    if(is.na(pos)) stop("Can't find ", op, " among coefficients ", paste(coefs, collapse=','))
    res[pos] <- 1
    return(res)
  } else if(is.numeric(op)) {
    return(matrix(c(rep(0,length(coefs)), -op), 1))
  } else {
    stop('Unkwnown item ',as.character(op), ' in formula ',formula)
  }
}

Rtoform <- function(R,r, coefs) {
  coefs <- gsub('`','',coefs,fixed=TRUE)
  form <- paste('~',paste(apply(R, 1, function(row) {
    w <- which(row != 0)
    rw <- paste(' ', row[w], '*`', coefs[w], '`', collapse=' + ', sep='')
    rw <- gsub('+ -',' - ',rw, fixed=TRUE)
    rw <- gsub(' 1*','',rw, fixed=TRUE)
    rw <- gsub('(fit)','',rw, fixed=TRUE)
    rw
  }), ' + ', -r, collapse='|', sep=''))
  form <- gsub('+ -','-',form, fixed=TRUE)
  form <- gsub(' 0.',' .',form, fixed=TRUE)
  form <- gsub('+ 0','',form, fixed=TRUE)
  local(as.formula(form))
}

# A modified estfun for felm objects. It multiplies the demeaned design matrix for the 
# second stage into the residuals. Also note, the felm object needs to be created with
# the argument keepCX=TRUE.
estfun.felm.mod <- function (fm, ...) 
{
    # Extract the weights from the felm object.
    wts <- fm$weights
    if(is.null(wts)) wts <- 1
    
    # If the felm object does IV, get residuals from the second stage.
    # Otherwise, get the normal residuals.
    # Either way, multiply into weights.
    if(is.null(fm$iv.residuals)) {
        res <- as.vector(fm$residuals) * wts
    } else {
        res <- as.vector(fm$iv.residuals) * wts
    }
    
  # multiply the residuals into the centered design matrix to get the 
  # empirical estimation function. Return it!
  rval <- as.matrix(res * fm$cX)
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  return(rval)
}
      
bread.mod <- function (fm) 
  {
    if (!is.null(fm$na.action)) 
    class(fm$na.action) <- "omit"
    cov.unscaled <- solve(t(fm$cX) %*% fm$cX)
    return(cov.unscaled)
}
 

vcov.adjacency.and.cluster.robust <- function(fm, adj_matrix, estfunc=estfun.felm.mod, n_ds=49) {
    
    #Renormalize weights so they sum to 1
    fm$weights <- sqrt((fm$weights^2/sum(fm$weights^2))*nrow(fm$cX))
    
    # Get estimating function
    eef <- estfunc(fm)
    
    # Produce cluster matrix
    cluster_matrix <- adj_matrix
    cluster_matrix[outer(floor(as.numeric(colnames(cluster_matrix))/1000), 
                         floor(as.numeric(colnames(cluster_matrix))/1000), "!=")] <- 0
    
    # Generate the first part of the meat
    fat_adj <- foreach(j=0:(n_ds-1), .combine='rbind') %do% {
        fat_adj_int <- do.call(cbind, 
                     c(rep(list(cluster_matrix), j), 
                       list(adj_matrix), 
                       rep(list(cluster_matrix), n_ds-1-j)
                      )
                    )
        
        #fat_adj_int <- do.call(cbind,
        #                  rep(list(adj_matrix), n_ds))
        #
        component <- fat_adj_int %*% eef
        component
    }
    
    N <- nrow(adj_matrix)
    
    # Calculate meat
    m = crossprod(eef, fat_adj)
    
    # Calculate bread
    b = bread.mod(fm)
    
    # Return sandwich estimator
    b %*% m %*% b
}

# A function to calculate the adjacency and cluster robust covariance matrix for a fitted felm object. 
# For panel data, the adjacency matrix is assumed to be a block diagonal matrix where each block is a matrix
# containing the element-wise max of the adjacency matrix and the cluster matrix. 
vcov.adjacency.robust <- function(fm, adjacency.matrix, estfunc=estfun.felm.mod) {
    # Calculate the empirical estimation function for the felm object.
    eef <- estfunc(fm)
    
    # Get the number of observations, as well as the 'meat' of the sandwich estimator.
    N <- nrow(adjacency.matrix)
    m <- crossprod(eef, adjacency.matrix %*% eef)
    
    # Get the sandwich estimator.
    sandwich(fm, meat = as.matrix(m) / N)
}


# This is a modification of the function to calculate a sandwich estimator. This is taken directly from Dean's
# 2016 PNAS code.
sandwich.mod <- function(x, bread. = bread, meat. = meat, ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  if (is.function(bread.))
    bread. <- bread.(x)
  if (is.function(meat.))
    meat. <- meat.(x, ...)

  n <- x$df.residual + x$rank
  return(1/n * (bread. %*% meat. %*% bread.))
}

# This is modified code to calculate cluster robust covariance matrix. Lightly edited from the one in 
# Dean's code. Not currently used, may be a bug.
#vcov.cluster.robust <- function(fm, clusters, estfunc=estfun.felm.mod) {
#    # Extract a bunch of information about the clusters.
#    if (!is.factor(clusters))
#        clusters <- factor(clusters)
#    M <- length(levels(clusters))
#    N <- length(clusters)
#    clusters <- as.integer(clusters)
#    
#    # Get the felm objects empirical estimation function.
#    eef <- estfunc(fm)
#    
#    # Calculate the degrees of freedom correction
#    dofc <- (M / (M - 1)) * ((N - 1) / (fm$df.residual))
#    
#    # Get the cluster level residuals
#    u <- foreach(i = 1:ncol(eef), .combine = cbind) %do% {
#        s <- vector("numeric", M)
#    
#        tapply(eef[,i], clusters, sum)
#    
#    }
#    
#    # Calculate the cluster-robust vcv matrix
#    vc <- dofc * sandwich.mod(fm, meat = crossprod(u) / N)
#    vc
#    
#}

# This is modified code from Dean's code to summarize the fit of a felm object. For panel data,
# the adjacency matrix is assumed to be a block diagonal matrix where each block is the adjacency 
# matrix at a given time period t in the panel.

summarize.felm.fit.with.adj <- function(fm, adj, clusters, estfunc=estfun.felm.mod, 
                                       xmat = NULL, n_ds = 49) {
    s <- list()

    # Calculate the empirical estimation function.
    if (is.null(xmat)) {
        eef <- estfunc(fm)
    } else {
        eef <- residuals(fm) * xmat
    }
    
    # Calculate cluster dof based on n_clusters
    cluster_dof <- length(unique(clusters))-1
    
    # Not using any of the code below
    # Calculate all three vcv matrices
    #s$vcov.adj <- vcov.adjacency.robust(fm, adj, estfunc=estfunc)
    #s$vcov.cluster <- fm$clustervcv
    #s$vcov.both <- max_elemwise(s$vcov.sand, s$vcov.cluster)
    #s$vcov.union <- vcov.union.robust(fm, adj, estfunc=estfunc)
    
    s$vcov.both <- vcov.adjacency.and.cluster.robust(fm, adj, estfunc=estfunc, n_ds)

    # check for negative eigenvalues in the union, and correct if necessary according to 
    # cameron, gelbach, miller.

    union_ev <- eigen(s$vcov.both)
    union_bad_ev <- Im(union_ev$values) != 0 | Re(union_ev$values) < 0
    if(any(union_bad_ev)) {
      union_ev$values[union_bad_ev] <- 0
      s$vcov.both <- Re(union_ev$vectors %*% diag(union_ev$values) %*% t(union_ev$vectors))
    }
    
    # All of these functions are not currently used
    #s$vcov.sand <- fm$robustvcv
    #s$vcov.sand <- sandwich.mod(fm)
    #s$vcov.cluster <- vcov.cluster.robust(fm, clusters, estfunc=estfunc)

    # # Calculate the combined vcv matrix.
    #s$vcov.both <- s$vcov.adj + s$vcov.cluster - s$vcov.sand

    # Do coef test and wald test
    s$ct <- coeftest(fm, vcov = as.matrix(s$vcov.both))
    s$wt <- felm.waldtest.mod(fm, 
                              R=rownames(fm$coefficients), 
                              vcov = s$vcov.both, 
                              df2=cluster_dof)
    
    s$residuals <- NULL
  # Profit
  s
}
