library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(lfe)
library(glmnet)
library(xgboost)
load('/nfs/sloanlab004/projects/covid_mobility_proj/data/intermediate_objects/residualizer_df.RData')
load('/nfs/sloanlab004/projects/covid_mobility_proj/data/intermediate_objects/folds.RData')
load('/nfs/sloanlab004/projects/covid_mobility_proj/data/PROCESSED_DATA/sciWM.RData')
load('/nfs/sloanlab004/projects/covid_mobility_proj/data/PROCESSED_DATA/geoWM.RData')

panel_residualizer <- function(Y) {
    residualizer_df %>%
        mutate(Y = Y,
               Y.r = felm(Y ~ 0 | county_fips + ds, .)$resid) -> temp_df
    
    dm    <- xgb.DMatrix(data = model.matrix(~ 0 + PRCP.fe.r + TMAX.fe.r, temp_df), label = temp_df$Y.r)
    param <- list(max_depth=2, eta=.5, silent=1, objective='reg:linear')
    fit   <- xgb.cv(params = param, 
                    data = dm, 
                    folds = folds,
                    nrounds = 100, 
                    early_stopping_rounds = 3, 
                    weight = temp_df$n)
    best_n <- fit$best_iteration
    for (i in 1:3) {
        tr  <- temp_df %>% filter(grp != i)
        trm <- xgb.DMatrix(data = model.matrix(~ 0 + PRCP.fe.r + TMAX.fe.r, tr), label = tr$Y.r)
        fit <- xgb.train(params = param, 
                         data = trm, 
                         nrounds = best_n, 
                         weight = tr$n)
        te  <- temp_df %>% filter(grp == i)
        tem <- xgb.DMatrix(data = model.matrix(~ 0 + PRCP.fe.r + TMAX.fe.r, te), label = te$Y.r)
        te %>%
            select(county_fips, ds) %>%
            mutate(pred = predict(fit, newdata = tem)) -> pred_df
        assign(str_c('temp',i), pred_df)
    }
    out <- bind_rows(temp1, temp2, temp3) %>%
        arrange(ds, county_fips) %>%
        mutate(resid = temp_df$Y.r - pred)
    return(out$resid)
}

# function that constructs alter covariate vectors
weightedAlters <- function(df, wm, ...) {
    df %>% 
        select(ds, county_fips, ...) %>%
        spread(key = county_fips, value = ...) %>%
        ungroup() %>%
        select(-ds) %>%
        as.matrix() -> txn_data
    
    df %>%
        ungroup() %>%
        select(ds) %>%
        distinct() %>%
        arrange(ds) -> dates
    
    outMatrix <- tcrossprod(txn_data, wm)
    colnames(outMatrix) <- colnames(txn_data)
    
    data.frame(dates, outMatrix) %>%
        gather(key = 'county_fips', value = 'value', -ds) %>%
        mutate(county_fips = as.integer(str_sub(county_fips, 2, -1))) %>% 
        arrange(ds, county_fips) %>%
        select(-ds, -county_fips) -> out_df
    return(out_df$value)
}