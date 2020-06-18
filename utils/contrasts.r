# code for hypothesis testing relating to various contrasts of interest
library(iterators)
# x1 and x2 are the matrices or data frames that include needed columns
# of the design matrix and differ in variables set differently by the policies of interest
# basically just may be an easier way to specify linear contrasts
policy_contrast_linear <- function(fm, x1, x2, weights, vcv = NULL) {
    if (is.null(vcv))
        vcv <- fm$clustervcv
    
    # to-do: allow x1 and x2 to be data frames that a model matrix
    # has to be built from (ie with interactions)
    dx <- apply(
        x1[, colnames(fm$cX)] - x2[, colnames(fm$cX)],
        2,
        weighted.mean,
        weights = weights
        )
    
    dx <- dx[dx != 0]
    
    form <- paste("~", paste(dx, "*", names(dx), collapse = " + "))
    
    result <- felm.waldtest.mod(
        fm,
        as.formula(form),
        vcov = vcv
    )
    #cat(print(result))
    result <- as.list(result)
    result$formula <- as.character(form)
    result$lhs <- as.character(fm$lhs)
    result
}

policy_contrast_linear_many_models <- function(fm_list, x1, x2, weights, vcv = NULL, ...) {
    foreach(
        m = fm_list, i=icount(),
        .combine = bind_rows) %do% {
    
        policy_contrast_linear(m, x1, x2, weights, vcv[[i]]) %>% as.data.frame()
} %>% mutate(...) #mutate(policy = policy_name)
}

# not currently used:
# adapted from https://github.com/skranz/regtools/blob/master/R/felm.r
# which doesn't actually work
# this only works with no endogenous variables
predict.felm.simple = function(object, newdata, ...) {
  co = coef(object)
  
  # too make code compatible with tibbles
  newdata = as.data.frame(newdata)
  
  rownames(newdata) = seq_along(newdata[,1])
  
  form = formula(object)
  # Need to extract part before first |
  # use brute force string manipulation
  library(Formula) # will be loaded by lfe anyways
  form.str = as.character(Formula(form))
  form.str = paste0(form.str[2], form.str[1], form.str[3])
    
  #cat(form.str)
  
  #form.str = capture.output(form)
  pos = regexpr("|", form.str, fixed=TRUE)
  if (pos > 0) {
    form.str = substr(form.str, 1, pos-1)
    form = as.formula(paste0(form.str, collapse = " "))
  }

  # model matrix
  mf = model.frame(form, newdata)
  mm = model.matrix(form, data = mf)
  #cat(colnames(mm))
  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept if not included in coefficients
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1,drop=FALSE]
  }
  y.pred = mm %*% co
  as.vector(y.pred)
}
