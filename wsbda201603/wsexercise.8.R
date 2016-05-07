source("lib.R")


#####
##### Exercise 8.2
#####

stan_file <- 'multicat_hierarchical.stan'

warmup_count <- 250
sample_count <- 250
chain_count  <- 8

ppc_stanmodel <- stan_model(stan_file, verbose = TRUE)

stan_data_lst <- list(n_cats     = inputdata_dt[, length(unique(prod_id))]
                     ,n_trials   = inputdata_dt[, length(unique(design_id))]
                     ,trials     = inputdata_dt$trial_count
                     ,success    = inputdata_dt$success
                     ,cat_id     = inputdata_dt$prod_id
                     ,priorShape = 1.1
                     ,priorRate  = 0.011
                     ,priorA     = 2
                     ,priorB     = 2
                      )

ppc_stanfit <- sampling(multipleprod_stanmodel
                                ,data      = stan_data_lst
                                ,algorithm = "NUTS"
                                ,warmup    = warmup_count
                                ,iter      = warmup_count + sample_count
                                ,chains    = chain_count
                                ,verbose   = TRUE
                                 )
