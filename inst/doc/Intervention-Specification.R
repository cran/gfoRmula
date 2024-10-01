## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE---------------------------------------------------------------
library('gfoRmula')
library('data.table')

## -----------------------------------------------------------------------------
id <- 'id'
time_points <- 7
time_name <- 't0'
covnames <- c('L1', 'L2', 'A')
outcome_name <- 'Y'
outcome_type <- 'survival'
covtypes <- c('binary', 'bounded normal', 'binary')
histories <- c(lagged, lagavg)
histvars <- list(c('A', 'L1', 'L2'), c('L1', 'L2'))
covparams <- list(covmodels = c(L1 ~ lag1_A + lag_cumavg1_L1 + lag_cumavg1_L2 +
                                  L3 + t0,
                                L2 ~ lag1_A + L1 + lag_cumavg1_L1 +
                                  lag_cumavg1_L2 + L3 + t0,
                                A ~ lag1_A + L1 + L2 + lag_cumavg1_L1 +
                                  lag_cumavg1_L2 + L3 + t0))
ymodel <- Y ~ A + L1 + L2 + L3 + lag1_A + lag1_L1 + lag1_L2 + t0
nsimul <- 10000

gform_basic <- gformula(obs_data = basicdata_nocomp, id = id,
                        time_points = time_points,
                        time_name = time_name, covnames = covnames,
                        outcome_name = outcome_name,
                        outcome_type = outcome_type, covtypes = covtypes,
                        covparams = covparams, ymodel = ymodel,
                        histories = histories, histvars = histvars,
                        basecovs = c('L3'), nsimul = nsimul,
                        seed = 1234, 
                        intervention1.A = list(static, rep(0, time_points)),
                        intervention2.A = list(static, rep(1, time_points)), 
                        int_descript = c('Never treat', 'Always treat'))
gform_basic

## -----------------------------------------------------------------------------
example_intervention <- function(newdf, pool, intvar, intvals, time_name, t){
  newdf[, (intvar) := 0]
  newdf[L2 < intvals[[1]], (intvar) := 1]
}

## -----------------------------------------------------------------------------
gform_basic <- gformula(obs_data = basicdata_nocomp, id = id,
                        time_points = time_points,
                        time_name = time_name, covnames = covnames,
                        outcome_name = outcome_name,
                        outcome_type = outcome_type, covtypes = covtypes,
                        covparams = covparams, ymodel = ymodel,
                        histories = histories, histvars = histvars,
                        basecovs = c('L3'), nsimul = nsimul,
                        seed = 1234, 
                        intervention1.A = list(example_intervention, 0.8),
                        intervention2.A = list(example_intervention, 1), 
                        int_descript = c('Treat if L2 < 0.8', 'Treat if L2 < 1'))
gform_basic

