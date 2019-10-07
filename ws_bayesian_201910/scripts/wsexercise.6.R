source("lib.R");


#####
##### Exercise 6.1
#####

bttt_dt <- readRDS("binarytrial_twotest.rds")

stan_file <- 'binarytrial_full.stan'

warmup_count <- 250
sample_count <- 1000
chain_count  <- 8

standata_lst <- list(n_tests = bttt_dt[, length(unique(trial_id))]
                    ,trials  = bttt_dt$trials
                    ,success = bttt_dt$success
                    ,priorShape = 10
                    ,priorRate  = 1
                     )

bttt_full_stanmodel <- stan_model(stan_file, verbose = TRUE)


bttt_full_stanfit <- sampling(bttt_full_stanmodel
                             ,data      = standata_lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup_count
                             ,iter      = warmup_count + sample_count
                             ,chains    = chain_count
                             ,verbose   = TRUE
                              )

if(exists("save_rds_files")) saveRDS(bttt_full_stanfit, file = 'cached_data/bttt_full_stanfit.rds', compress = 'xz')

monitor(bttt_full_stanfit, probs = c(0.1,0.5,0.9))
traceplot(bttt_full_stanfit, pars = c("mu", "theta", "K")) + expand_limits(y = c(0,1))


#####
##### Exercise 6.3
#####

trial5_dt <- generate_trial_data(c(0.4, 0.45, 0.50, 0.45, 0.55)
                                ,c( 50,  100,   75,  125,  150))

standata_lst <- list(n_tests    = trial5_dt[, length(unique(test_id))]
                    ,trials     = trial5_dt$trials
                    ,success    = trial5_dt$success
                    ,priorShape = 10
                    ,priorRate  = 1
                     )

trial_var_stanmodel <- stan_model(stan_file, verbose = TRUE)


trial_var_stanfit <- sampling(trial_var_stanmodel
                             ,data      = standata_lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup_count
                             ,iter      = warmup_count + sample_count
                             ,chains    = chain_count
                             ,verbose   = TRUE
                              )

if(exists("save_rds_files")) saveRDS(trial_var_stanfit, file = 'cached_data/trial_var_stanfit.rds', compress = 'xz')

monitor(trial_var_stanfit, probs = c(0.1,0.5,0.9))
traceplot(trial_var_stanfit, pars = c("mu", "theta", "K")) + expand_limits(y = c(0,1))


#####
##### Exercise 6.7
#####

binomial_050_dt <- generate_hierarchical_binomial_data(tests = 50,  total_trials = 2500)
binomial_500_dt <- generate_hierarchical_binomial_data(tests = 500, total_trials = 2500)

binomial_alt_stanmodel <- stan_model(stan_file, verbose = TRUE)

standata_lst <- list(n_tests = binomial_050_dt[, length(unique(test_id))]
                    ,trials  = binomial_050_dt$trials
                    ,success = binomial_050_dt$success
                    ,priorShape = 10
                    ,priorRate  = 1
                     )

binomial_var050_stanfit <- sampling(binomial_alt_stanmodel
                                   ,data      = standata_lst
                                   ,algorithm = "NUTS"
                                   ,warmup    = warmup_count
                                   ,iter      = warmup_count + sample_count
                                   ,chains    = chain_count
                                   ,verbose   = TRUE
                                    )

if(exists("save_rds_files")) saveRDS(binomial_var050_stanfit, file = 'cached_data/binomial_var050_stanfit.rds', compress = 'xz')

monitor(binomial_var050_stanfit, probs = c(0.1,0.5,0.9))

traceplot(binomial_var050_stanfit, pars = c("mu", "K")) +
    expand_limits(y = c(0,1)) +
    ggtitle("Stan Output for 500 Coins, 50 Tosses - (mu = 0.5, K = 20)")


standata_lst <- list(n_tests = binomial_500_dt[, length(unique(test_id))]
                    ,trials  = binomial_500_dt$trials
                    ,success = binomial_500_dt$success
                    ,priorShape = 10
                    ,priorRate  = 1
                     )

binomial_var500_stanfit <- sampling(binomial_alt_stanmodel
                                   ,data      = standata_lst
                                   ,algorithm = "NUTS"
                                   ,warmup    = warmup_count
                                   ,iter      = warmup_count + sample_count
                                   ,chains    = chain_count
                                   ,verbose   = TRUE
                                    )

if(exists("save_rds_files")) saveRDS(binomial_var500_stanfit, file = 'cached_data/binomial_var500_stanfit.rds', compress = 'xz')

monitor(binomial_var500_stanfit, probs = c(0.1,0.5,0.9))
traceplot(binomial_var500_stanfit, pars = c("mu", "K")) +
    expand_limits(y = c(0,1)) +
    ggtitle("Stan Output for 50 Coins, 250 Tosses - (mu = 0.5, K = 20)")
