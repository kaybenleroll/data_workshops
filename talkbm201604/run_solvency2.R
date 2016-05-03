library(ggplot2)
library(data.table)
library(scales)
library(rstan)


ppauto_dt <- fread("data/ppauto_pos.csv")

use_grcode <- c(43,353,388,620)

ppauto_ss_dt <- ppauto_dt[GRCODE %in% use_grcode
                        ][DevelopmentYear < 1998
                        ][, .(grcode     = GRCODE
                             ,accyear    = AccidentYear
                             ,devlag     = DevelopmentLag
                             ,premium    = EarnedPremDIR_B
                             ,cumloss    = CumPaidLoss_B
                             ,loss_ratio = CumPaidLoss_B / EarnedPremDIR_B)]

ss_dt <- ppauto_ss_dt[grcode == 43]

cohort_dt <- ss_dt[, .(maxtime = max(devlag), premium = unique(premium)), by = accyear]
cohort_dt[, cohort_id := .I]

lst_standata <- list(growthmodel_id = 1   # Use weibull model
                    ,n_data         = ss_dt[, .N]
                    ,n_time         = ss_dt[, length(unique(devlag))]
                    ,n_cohort       = cohort_dt[, .N]
                    ,cohort_id      = match(ss_dt$accyear, cohort_dt$accyear)
                    ,t_value        = ss_dt[, sort(unique(devlag))]
                    ,t_idx          = ss_dt[, match(devlag, sort(unique(devlag)))]
                    ,premium        = cohort_dt$premium
                    ,loss           = ss_dt$cumloss
                    ,cohort_maxtime = cohort_dt$maxtime
                    )

lgm_1_stanmodel <- stan_model('losscurves_single.stan', verbose = TRUE)

lgm_1_stanfit <- sampling(lgm_1_stanmodel
                         ,data    = lst_standata
                         ,iter    = 1000
                         ,chains  = 8
                         ,verbose = TRUE
                          )

solv2_sample <- sapply(1:200, function(idx) {
    temp_fit <- sampling(lgm_1_stanmodel
                        ,data    = lst_standata
                        ,iter    = 1000
                        ,chains  = 8
                        ,verbose = TRUE
                         )

    iter_sample <- quantile(extract(temp_fit)$loss_prediction[, 8, 4], 0.995)

    return(iter_sample)
})
