source("lib.R");


#####
##### Exercise 6.1
#####

smtc.dt <- readRDS("singlemint_twocoin.rds")

stan.file <- 'singlemint_full.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(n_coins = smtc.dt[, length(unique(coin.id))]
                     ,trials  = smtc.dt$trials
                     ,success = smtc.dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

coin.full.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.full.stanfit <- sampling(coin.full.stanmodel
                             ,data      = stan.data.lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup.count
                             ,iter      = warmup.count + sample.count
                             ,chains    = chain.count
                             ,verbose   = TRUE
                              )

traceplot(coin.full.stanfit) + expand_limits(y = c(0,1))


#####
##### Exercise 6.3
#####

coin5.dt <- generate.coin.data(c(0.4, 0.45, 0.50, 0.45, 0.55)
                              ,c(50,   100,   75,  125,  150))

stan.data.lst <- list(n_coins = coin5.dt[, length(unique(coin.id))]
                     ,trials  = coin5.dt$trials
                     ,success = coin5.dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

coin.tossvar.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.tossvar.stanfit <- sampling(coin.tossvar.stanmodel
                                ,data      = stan.data.lst
                                ,algorithm = "NUTS"
                                ,warmup    = warmup.count
                                ,iter      = warmup.count + sample.count
                                ,chains    = chain.count
                                ,verbose   = TRUE
                                 )

traceplot(coin.tossvar.stanfit) + expand_limits(y = c(0,1))


#####
##### Exercise 6.7
#####

coin_05.dt <- generate.hierarchical.coin.data(coins = 5)
coin_50.dt <- generate.hierarchical.coin.data(coins = 50)

coin.alt.stanmodel <- stan_model(stan.file, verbose = TRUE)

stan.data.lst <- list(n_coins = coin_05.dt[, length(unique(coin.id))]
                     ,trials  = coin_05.dt$trials
                     ,success = coin_05.dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

coin.var05.stanfit <- sampling(coin.alt.stanmodel
                              ,data      = stan.data.lst
                              ,algorithm = "NUTS"
                              ,warmup    = warmup.count
                              ,iter      = warmup.count + sample.count
                              ,chains    = chain.count
                              ,verbose   = TRUE
                               )

traceplot(coin.var05.stanfit) + expand_limits(y = c(0,1))



stan.data.lst <- list(n_coins = coin_50.dt[, length(unique(coin.id))]
                     ,trials  = coin_50.dt$trials
                     ,success = coin_50.dt$success
                     ,priorShape = 10
                     ,priorRate  = 1
                      )

coin.var50.stanfit <- sampling(coin.alt.stanmodel
                              ,data      = stan.data.lst
                              ,algorithm = "NUTS"
                              ,warmup    = warmup.count
                              ,iter      = warmup.count + sample.count
                              ,chains    = chain.count
                              ,verbose   = TRUE
                               )

traceplot(coin.var50.stanfit) + expand_limits(y = c(0,1))
