source("lib.R");


#####
##### Exercise 7.1
#####

set.seed(42)

mint.mu <- c(0.48, 0.51, 0.47, 0.51, 0.53)
mint.K  <- c(1000,  750,  500, 1500, 4000)

coin.mean <-  50
coin.sd   <-  10

toss.mean <- 250
toss.sd   <-  50

multiple.dt  <- generate.multiple.mint.data(mint.mu, mint.K, coin.mean, coin.sd, toss.mean, toss.sd)
inputdata.dt <- multiple.dt[, .(mint.id, coin.id, toss.count, success)]


#####
##### Exercise 7.2
#####



#####
##### Exercise 7.3
#####

stan.file <- 'multiplemint_hierarchical.stan'

warmup.count <- 250
sample.count <- 250
chain.count  <- 8

multiplemint.stanmodel <- stan_model(stan.file, verbose = TRUE)

stan.data.lst <- list(n_mints  = inputdata.dt[, length(unique(mint.id))]
                     ,n_coins  = inputdata.dt[, length(unique(coin.id))]
                     ,trials   = inputdata.dt$toss.count
                     ,success  = inputdata.dt$success
                     ,mint_id  = inputdata.dt$mint.id
                     ,priorShape = 1.1
                     ,priorRate  = 0.0011
                     ,priorA     = 2
                     ,priorB     = 2
                      )

multiplemint.stanfit <- sampling(multiplemint.stanmodel
                                ,data      = stan.data.lst
                                ,algorithm = "NUTS"
                                ,warmup    = warmup.count
                                ,iter      = warmup.count + sample.count
                                ,chains    = chain.count
                                ,verbose   = TRUE
                                 )


#####
##### Exercise 7.4
#####

stan.file <- 'multiplemint_lognormal_k.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

multiplemint.lognorm.stanmodel <- stan_model(stan.file, verbose = TRUE)

stan.data.lst <- list(n_mints  = inputdata.dt[, length(unique(mint.id))]
                     ,n_coins  = inputdata.dt[, length(unique(coin.id))]
                     ,trials   = inputdata.dt$toss.count
                     ,success  = inputdata.dt$success
                     ,mint_id  = inputdata.dt$mint.id
                     ,priorMean = 8
                     ,priorSD   = 0.5
                     ,priorA    = 2
                     ,priorB    = 2
                      )

multiplemint.lognorm.stanfit <- sampling(multiplemint.lognorm.stanmodel
                                        ,data      = stan.data.lst
                                        ,algorithm = "NUTS"
                                        ,warmup    = warmup.count
                                        ,iter      = warmup.count + sample.count
                                        ,chains    = chain.count
                                        ,verbose   = TRUE
                                         )


#####
##### Exercise 7.5
#####

warmup.count <- 250
sample.count <- 250
chain.count  <- 8

coin.mean <- 500
coin.sd   <-  50

toss.mean <- 1000
toss.sd   <-   50

multiple.more.dt <- generate.multiple.mint.data(mint.mu, mint.K, coin.mean, coin.sd, toss.mean, toss.sd)
inputdata.dt <- multiple.more.dt[, .(mint.id, coin.id, toss.count, success)]

stan.data.lst <- list(n_mints  = inputdata.dt[, length(unique(mint.id))]
                     ,n_coins  = inputdata.dt[, length(unique(coin.id))]
                     ,trials   = inputdata.dt$toss.count
                     ,success  = inputdata.dt$success
                     ,mint_id  = inputdata.dt$mint.id
                     ,priorShape = 1.1
                     ,priorRate  = 0.0011
                     ,priorA     = 2
                     ,priorB     = 2
                      )

multiplemint.more.stanfit <- sampling(multiplemint.stanmodel
                                     ,data      = stan.data.lst
                                     ,algorithm = "NUTS"
                                     ,warmup    = warmup.count
                                     ,iter      = warmup.count + sample.count
                                     ,chains    = chain.count
                                     ,verbose   = TRUE
                                      )


#####
##### Exercise 7.6
#####

coin.mean <- 500
coin.sd   <-  50

toss.mean <- 250
toss.sd   <-  50

multiple.lowtoss.dt <- generate.multiple.mint.data(mint.mu, mint.K, coin.mean, coin.sd, toss.mean, toss.sd)
inputdata.dt <- multiple.lowtoss.dt[, .(mint.id, coin.id, toss.count, success)]

stan.data.lst <- list(n_mints  = inputdata.dt[, length(unique(mint.id))]
                     ,n_coins  = inputdata.dt[, length(unique(coin.id))]
                     ,trials   = inputdata.dt$toss.count
                     ,success  = inputdata.dt$success
                     ,mint_id  = inputdata.dt$mint.id
                     ,priorShape = 1.1
                     ,priorRate  = 0.0011
                     ,priorA     = 2
                     ,priorB     = 2
                      )

multiplemint.lowtoss.stanfit <- sampling(multiplemint.stanmodel
                                        ,data      = stan.data.lst
                                        ,algorithm = "NUTS"
                                        ,warmup    = warmup.count
                                        ,iter      = warmup.count + sample.count
                                        ,chains    = chain.count
                                        ,verbose   = TRUE
                                         )


coin.mean <- 250
coin.sd   <-  50

toss.mean <- 1000
toss.sd   <-   50

multiple.lowcoin.dt <- generate.multiple.mint.data(mint.mu, mint.K, coin.mean, coin.sd, toss.mean, toss.sd)
inputdata.dt <- multiple.lowcoin.dt[, .(mint.id, coin.id, toss.count, success)]

stan.data.lst <- list(n_mints  = inputdata.dt[, length(unique(mint.id))]
                     ,n_coins  = inputdata.dt[, length(unique(coin.id))]
                     ,trials   = inputdata.dt$toss.count
                     ,success  = inputdata.dt$success
                     ,mint_id  = inputdata.dt$mint.id
                     ,priorShape = 1.1
                     ,priorRate  = 0.0011
                     ,priorA     = 2
                     ,priorB     = 2
                      )

multiplemint.lowcoin.stanfit <- sampling(multiplemint.stanmodel
                                        ,data      = stan.data.lst
                                        ,algorithm = "NUTS"
                                        ,warmup    = warmup.count
                                        ,iter      = warmup.count + sample.count
                                        ,chains    = chain.count
                                        ,verbose   = TRUE
                                         )
