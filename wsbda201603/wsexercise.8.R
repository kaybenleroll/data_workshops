source("lib.R")


#####
##### Exercise 8.2
#####

set.seed(42)

prod_mu <- c(0.08, 0.11, 0.09, 0.11, 0.10)
prod_K  <- c( 200,  250,  150,  200,  400)

design_mean <- 200
design_sd   <-  10

trial_mean <- 1000
trial_sd   <-   50

multiple_dt  <- generate_multiple_hier_trial_data(prod_mu, prod_K, design_mean, design_sd, trial_mean, trial_sd)
inputdata_dt <- multiple_dt[, .(prod_id = cat_id, design_id = trial_id, trial_count, success)]


stan_file <- 'multipleprod_hierarchical.stan'

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

ppc_stanfit <- sampling(ppc_stanmodel
                       ,data      = stan_data_lst
                       ,algorithm = "NUTS"
                       ,warmup    = warmup_count
                       ,iter      = warmup_count + sample_count
                       ,chains    = chain_count
                       ,verbose   = TRUE
                        )

if(exists("save_rds_files")) saveRDS(ppc_stanfit
                                    ,file = 'cached_data/ppc_stanfit.rds'
                                    ,compress = 'xz')

ppcdata_dt <- inputdata_dt[, .(mintheta = min(success / trial_count)
                              ,maxtheta = max(success / trial_count))
                           ,by = prod_id][, .(Var2 = prod_id, mintheta, maxtheta)]

ggplot(data = melt(extract(ppc_stanfit)$mintheta)) +
    stat_density(aes(x = value), geom = 'line') +
    geom_vline(aes(xintercept = mintheta), colour = 'red', data = ppcdata_dt) +
    expand_limits(x = 0) +
    facet_wrap(~Var2)

ggplot(data = melt(extract(ppc_stanfit)$maxtheta)) +
    stat_density(aes(x = value), geom = 'line') +
    geom_vline(aes(xintercept = maxtheta), colour = 'red', data = ppcdata_dt) +
    expand_limits(x = 0.10) +
    facet_wrap(~Var2)
