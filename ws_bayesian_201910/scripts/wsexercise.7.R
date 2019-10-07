source("lib.R")


#####
##### Exercise 7.1
#####

set.seed(4242)

prod_mu <- c(0.08, 0.11, 0.09, 0.11, 0.10)
prod_K  <- c( 200,  250,  150,  200,  400)

design_mean <-  50
design_sd   <-  10

trial_mean <- 250
trial_sd   <-  50

multiple_dt  <- generate_multiple_hier_trial_data(prod_mu, prod_K, design_mean, design_sd, trial_mean, trial_sd)
inputdata_dt <- multiple_dt[, .(prod_id = cat_id, design_id = trial_id, trial_count, success)]


#####
##### Exercise 7.2
#####



#####
##### Exercise 7.3
#####

stan_file <- 'multipleprod_hierarchical.stan'

warmup_count <- 250
sample_count <- 250
chain_count  <- 8

multipleprod_stanmodel <- stan_model(stan_file, verbose = TRUE)

standata_lst <- list(n_cats     = inputdata_dt[, length(unique(prod_id))]
                    ,n_trials   = inputdata_dt[, length(unique(design_id))]
                    ,trials     = inputdata_dt$trial_count
                    ,success    = inputdata_dt$success
                    ,cat_id     = inputdata_dt$prod_id
                    ,priorShape = 1.1
                    ,priorRate  = 0.011
                    ,priorA     = 2
                    ,priorB     = 2
                     )

multipleprod_stanfit <- sampling(multipleprod_stanmodel
                                ,data      = standata_lst
                                ,algorithm = "NUTS"
                                ,warmup    = warmup_count
                                ,iter      = warmup_count + sample_count
                                ,chains    = chain_count
                                ,verbose   = TRUE
                                 )

if(exists("save_rds_files")) saveRDS(multipleprod_stanfit
                                    ,file = 'cached_data/multipleprod_stanfit.rds'
                                    ,compress = 'xz')

monitor(multipleprod_stanfit, probs = c(0.1,0.5,0.9), digits_summary = 3)
traceplot(multipleprod_stanfit, pars = c("mu","K")) + expand_limits(y = c(0,0.15))



#####
##### Exercise 7.4
#####

stan_file <- 'multipleprod_lognormal_k.stan'

warmup_count <- 250
sample_count <- 1000
chain_count  <- 8

multipleprod_lognorm_stanmodel <- stan_model(stan_file, verbose = TRUE)

standata_lst <- list(n_cats    = inputdata_dt[, length(unique(prod_id))]
                    ,n_trials  = inputdata_dt[, length(unique(design_id))]
                    ,trials    = inputdata_dt$trial_count
                    ,success   = inputdata_dt$success
                    ,cat_id    = inputdata_dt$prod_id
                    ,priorMean = 8
                    ,priorSD   = 0.5
                    ,priorA    = 2
                    ,priorB    = 2
                     )

multipleprod_lognorm_stanfit <- sampling(multipleprod_lognorm_stanmodel
                                        ,data      = standata_lst
                                        ,algorithm = "NUTS"
                                        ,warmup    = warmup_count
                                        ,iter      = warmup_count + sample_count
                                        ,chains    = chain_count
                                        ,verbose   = TRUE
                                         )

if(exists("save_rds_files")) saveRDS(multipleprod_lognorm_stanfit
                                    ,file = 'cached_data/multipleprod_lognorm_stanfit.rds'
                                    ,compress = 'xz')

monitor(multipleprod_lognorm_stanfit, probs = c(0.1,0.5,0.9), digits_summary = 3)
traceplot(multipleprod_lognorm_stanfit, pars = c("mu","K")) + expand_limits(y = c(0,0.15))


#####
##### Exercise 7.5
#####

###
### WARNING!!! I've had convergence issues with this next part and have not
### been able to figure out why. It may be a good thing to explore in terms
### of diagnostics etc.
###
### This could be just an artefact of some weird data we generated.
###

warmup_count <- 250
sample_count <- 250
chain_count  <- 8

design_mean <- 500
design_sd   <-  50

toss_mean <- 1000
toss_sd   <-   50

stan_file <- 'multipleprod_hierarchical.stan'

multipleprod_stanmodel <- stan_model(stan_file, verbose = TRUE)

multiple_more_dt <- generate_multiple_hier_trial_data(prod_mu, prod_K, design_mean, design_sd, toss_mean, toss_sd)
inputdata_dt     <- multiple_more_dt[, .(prod_id = cat_id, design_id = trial_id, trial_count, success)]

standata_lst <- list(n_cats     = inputdata_dt[, length(unique(prod_id))]
                    ,n_trials   = inputdata_dt[, length(unique(design_id))]
                    ,trials     = inputdata_dt$trial_count
                    ,success    = inputdata_dt$success
                    ,cat_id     = inputdata_dt$prod_id
                    ,priorShape = 1.1
                    ,priorRate  = 0.0011
                    ,priorA     = 2
                    ,priorB     = 2
                     )

multipleprod_more_stanfit <- sampling(multipleprod_stanmodel
                                     ,data      = standata_lst
                                     ,algorithm = "NUTS"
                                     ,warmup    = warmup_count
                                     ,iter      = warmup_count + sample_count
                                     ,chains    = chain_count
                                     ,verbose   = TRUE
                                      )

if(exists("save_rds_files")) saveRDS(multipleprod_more_stanfit
                                    ,file = 'cached_data/multipleprod_more_stanfit.rds'
                                    ,compress = 'xz')

monitor(multipleprod_more_stanfit, probs = c(0.1,0.5,0.9), digits_summary = 3)
traceplot(multipleprod_more_stanfit, pars = c("mu","K")) + expand_limits(y = c(0,0.15))


#####
##### Exercise 7.6
#####

design_mean <- 500
design_sd   <-  50

toss_mean <- 250
toss_sd   <-  50

multiple_lowtoss_dt <- generate_multiple_hier_trial_data(prod_mu, prod_K, design_mean, design_sd, toss_mean, toss_sd)
inputdata_dt     <- multiple_lowtoss_dt[, .(prod_id = cat_id, design_id = trial_id, trial_count, success)]

standata_lst <- list(n_cats     = inputdata_dt[, length(unique(prod_id))]
                    ,n_trials   = inputdata_dt[, length(unique(design_id))]
                    ,trials     = inputdata_dt$trial_count
                    ,success    = inputdata_dt$success
                    ,cat_id     = inputdata_dt$prod_id
                    ,priorShape = 1.1
                    ,priorRate  = 0.0011
                    ,priorA     = 2
                    ,priorB     = 2
                     )

multipleprod_lowtoss_stanfit <- sampling(multipleprod_stanmodel
                                        ,data      = standata_lst
                                        ,algorithm = "NUTS"
                                        ,warmup    = warmup_count
                                        ,iter      = warmup_count + sample_count
                                        ,chains    = chain_count
                                        ,verbose   = TRUE
                                         )

if(exists("save_rds_files")) saveRDS(multipleprod_lowtoss_stanfit
                                    ,file = 'cached_data/multipleprod_lowtoss_stanfit.rds'
                                    ,compress = 'xz')

monitor(multipleprod_lowtoss_stanfit, probs = c(0.1,0.5,0.9), digits_summary = 3)
traceplot(multipleprod_lowtoss_stanfit, pars = c("mu","K")) + expand_limits(y = c(0,0.15))



design.mean <- 250
design.sd   <-  50

toss.mean <- 1000
toss.sd   <-   50

multiple_lowdesign_dt <- generate_multiple_hier_trial_data(prod_mu, prod_K, design_mean, design_sd, toss_mean, toss_sd)
inputdata_dt          <- multiple_lowdesign_dt[, .(prod_id = cat_id, design_id = trial_id, trial_count, success)]

standata_lst <- list(n_cats     = inputdata_dt[, length(unique(prod_id))]
                    ,n_trials   = inputdata_dt[, length(unique(design_id))]
                    ,trials     = inputdata_dt$trial_count
                    ,success    = inputdata_dt$success
                    ,cat_id     = inputdata_dt$prod_id
                    ,priorShape = 1.1
                    ,priorRate  = 0.0011
                    ,priorA     = 2
                    ,priorB     = 2
                     )

multipleprod_lowdesign_stanfit <- sampling(multipleprod_stanmodel
                                          ,data      = standata_lst
                                          ,algorithm = "NUTS"
                                          ,warmup    = warmup_count
                                          ,iter      = warmup_count + sample_count
                                          ,chains    = chain_count
                                          ,verbose   = TRUE
                                           )

if(exists("save_rds_files")) saveRDS(multipleprod_lowdesign_stanfit
                                    ,file = 'cached_data/multipleprod_lowdesign_stanfit.rds'
                                    ,compress = 'xz')

monitor(multipleprod_lowdesign_stanfit, probs = c(0.1,0.5,0.9), digits_summary = 3)
traceplot(multipleprod_lowdesign_stanfit, pars = c("mu","K")) + expand_limits(y = c(0,0.15))
