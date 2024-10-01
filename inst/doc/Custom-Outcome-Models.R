## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
ymodel_fit_custom <- function(ymodel, obs_data){
  return(randomForest::randomForest(formula = ymodel, data = obs_data))
}

## -----------------------------------------------------------------------------
ymodel_predict_custom <- function(fit, newdf){
  return(as.numeric(predict(object = fit, newdata = newdf)))
}

## ----echo=FALSE---------------------------------------------------------------
library('gfoRmula')
library('data.table')

## -----------------------------------------------------------------------------
library('Hmisc')
id <- 'id'
time_name <- 't0'
covnames <- c('L1', 'L2', 'A')
outcome_name <- 'Y'
outcome_type <- 'continuous_eof'
covtypes <- c('categorical', 'normal', 'binary')
histories <- c(lagged)
histvars <- list(c('A', 'L1', 'L2'))
covparams <- list(covmodels = c(L1 ~ lag1_A + lag1_L1 + L3 + t0 +
                                  rcspline.eval(lag1_L2, knots = c(-1, 0, 1)),
                                L2 ~ lag1_A + L1 + lag1_L1 + lag1_L2 + L3 + t0,
                                A ~ lag1_A + L1 + L2 + lag1_L1 + lag1_L2 + L3 + t0))
ymodel <- Y ~ A + L1 + L2 + lag1_A + lag1_L1 + lag1_L2 + L3
intervention1.A <- list(static, rep(0, 7))
intervention2.A <- list(static, rep(1, 7))
int_descript <- c('Never treat', 'Always treat')
nsimul <- 10000

gform_cont_eof <- gformula(obs_data = continuous_eofdata,
                           id = id, time_name = time_name,
                           covnames = covnames, outcome_name = outcome_name,
                           outcome_type = outcome_type, covtypes = covtypes,
                           covparams = covparams, ymodel = ymodel,
                           ymodel_fit_custom = ymodel_fit_custom, 
                           ymodel_predict_custom = ymodel_predict_custom,
                           intervention1.A = intervention1.A,
                           intervention2.A = intervention2.A,
                           int_descript = int_descript,
                           histories = histories, histvars = histvars,
                           basecovs = c("L3"), nsimul = nsimul, seed = 1234)
gform_cont_eof

