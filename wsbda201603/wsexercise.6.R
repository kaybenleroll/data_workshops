source("lib.R");


#####
##### Exercise 6.1
#####

bttt_dt <- readRDS("binarytrial_twotest.rds")

stan_file <- 'binarytrial_full.stan'

warmup_count <- 250
sample_count <- 1000
chain_count  <- 8

stan_data_lst <- list(n_coins = bttt_dt[, length(unique(trial_id))]
                     ,trials  = bttt_dt$trials
                     ,success = bttt_dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

bttt_full_stanmodel <- stan_model(stan_file, verbose = TRUE)


bttt_full_stanfit <- sampling(bttt_full_stanmodel
                             ,data      = stan_data_lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup_count
                             ,iter      = warmup_count + sample_count
                             ,chains    = chain_count
                             ,verbose   = TRUE
                              )

traceplot(bttt_full_stanfit) + expand_limits(y = c(0,1))


#####
##### Exercise 6.3
#####

trial5_dt <- generate_trial_data(c(0.4, 0.45, 0.50, 0.45, 0.55)
                                ,c( 50,  100,   75,  125,  150))

stan_data_lst <- list(n_tests    = trial5_dt[, length(unique(test_id))]
                     ,trials     = trial5_dt$trials
                     ,success    = trial5_dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

trial_var_stanmodel <- stan_model(stan_file, verbose = TRUE)


trial_var_stanfit <- sampling(trial_var_stanmodel
                             ,data      = stan_data_lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup_count
                             ,iter      = warmup_count + sample_count
                             ,chains    = chain_count
                             ,verbose   = TRUE
                              )

traceplot(trial_var_stanfit) + expand_limits(y = c(0,1))


#####
##### Exercise 6.7
#####

binomial_05_dt <- generate_hierarchical_binomial_data(tests = 5)
binomial_50_dt <- generate_hierarchical_binomial_data(tests = 50)

binomial_alt_stanmodel <- stan_model(stan_file, verbose = TRUE)

stan_data_lst <- list(n_tests = binomial_05_dt[, length(unique(test_id))]
                     ,trials  = binomial_05_dt$trials
                     ,success = binomial_05_dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

binomial_var05_stanfit <- sampling(binomial_alt_stanmodel
                                  ,data      = stan_data_lst
                                  ,algorithm = "NUTS"
                                  ,warmup    = warmup_count
                                  ,iter      = warmup_count + sample_count
                                  ,chains    = chain_count
                                  ,verbose   = TRUE
                                   )

traceplot(binomial_var05_stanfit) + expand_limits(y = c(0,1))



stan_data_lst <- list(n_tests = binomial_50_dt[, length(unique(test_id))]
                     ,trials  = binomial_50_dt$trials
                     ,success = binomial_50_dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

binomial_var50_stanfit <- sampling(binomial_alt_stanmodel
                              ,data      = stan_data_lst
                              ,algorithm = "NUTS"
                              ,warmup    = warmup_count
                              ,iter      = warmup_count + sample_count
                              ,chains    = chain_count
                              ,verbose   = TRUE
                               )

traceplot(binomial_var50_stanfit) + expand_limits(y = c(0,1))
