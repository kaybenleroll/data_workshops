source("lib.R");


#####
##### Exercise 5.1
#####

cointoss10 <- readRDS("cointoss10.rds")

stan.file <- 'singlemint_singlecoin.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(y = cointoss10
                     ,N = length(cointoss10)
                      )

coin.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.stanfit <- sampling(coin.stanmodel
                        ,data      = stan.data.lst
                        ,algorithm = "NUTS"
                        ,warmup    = warmup.count
                        ,iter      = warmup.count + sample.count
                        ,chains    = chain.count
                        ,verbose   = TRUE
                         )

traceplot(coin.stanfit)


#####
##### Exercise 5.3
#####

theta.seq <- seq(0, 1, by = 0.001)
H.10.count <- sum(cointoss10)
T.10.count <- length(cointoss10) - H.10.count

beta.prior.0202 <- dbeta(theta.seq, 2, 2)
beta.posterior  <- dbeta(theta.seq, 2 + H.10.count, 2 + T.10.count)


qplot(extract(coin.stanfit)$theta, geom = 'density') +
    geom_line(aes(x = theta.seq, y = beta.prior.0202), colour = 'red') +
    geom_line(aes(x = theta.seq, y = beta.posterior),  colour = 'blue') +
    xlab(expression(theta))



#####
##### Exercise 5.4
#####

cointoss10 <- readRDS("cointoss10.rds")

stan.file <- 'singlemint_singlecoin_flexible.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(y      = cointoss10
                     ,N      = length(cointoss10)
                     ,priorA = 5
                     ,priorB = 5
                      )

coin.flex.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.flex.stanfit <- sampling(coin.stanmodel
                             ,data      = stan.data.lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup.count
                             ,iter      = warmup.count + sample.count
                             ,chains    = chain.count
                             ,verbose   = TRUE
                              )


beta.prior.0505 <- dbeta(theta.seq, 5, 5)
beta.posterior  <- dbeta(theta.seq, 5 + H.10.count, 5 + T.10.count)


qplot(extract(coin.stanfit)$theta, geom = 'density') +
    geom_line(aes(x = theta.seq, y = beta.prior.0505), colour = 'red') +
    geom_line(aes(x = theta.seq, y = beta.posterior),  colour = 'blue') +
    xlab(expression(theta))


#####
##### Exercise 5.6
#####

smtc.dt <- readRDS("singlemint_twocoin.rds")

stan.file <- 'singlemint_multiplecoin.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(n_coins = smtc.dt[, length(unique(coin.id))]
                     ,K = 5
                     ,trials  = smtc.dt$trials
                     ,success = smtc.dt$success
                      )

coin.smtc.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.smtc.stanfit <- sampling(coin.smtc.stanmodel
                             ,data      = stan.data.lst
                             ,algorithm = "NUTS"
                             ,warmup    = warmup.count
                             ,iter      = warmup.count + sample.count
                             ,chains    = chain.count
                             ,verbose   = TRUE
                              )

traceplot(coin.smtc.stanfit) + expand_limits(y = c(0,1))


#####
##### Exercise 5.7
#####

smtc.dt <- readRDS("singlemint_twocoin.rds")

stan.file <- 'singlemint_multiplecoin.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(n_coins = smtc.dt[, length(unique(coin.id))]
                     ,K = 100
                     ,trials  = smtc.dt$trials
                     ,success = smtc.dt$success
                      )

coin.K_100.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.K_100.stanfit <- sampling(coin.K_100.stanmodel
                              ,data      = stan.data.lst
                              ,algorithm = "NUTS"
                              ,warmup    = warmup.count
                              ,iter      = warmup.count + sample.count
                              ,chains    = chain.count
                              ,verbose   = TRUE
                               )

traceplot(coin.K_100.stanfit) + expand_limits(y = c(0,1))



#####
##### Exercise 5.8
#####

coin5.dt <- generate.coin.data(c(0.4, 0.45, 0.50, 0.45, 0.55), 50)

stan.file <- 'singlemint_multiplecoin.stan'

warmup.count <- 250
sample.count <- 1000
chain.count  <- 8

stan.data.lst <- list(n_coins = smtc.dt[, length(unique(coin.id))]
                     ,K = 5
                     ,trials  = smtc.dt$trials
                     ,success = smtc.dt$success
                      )

coin.coin5.stanmodel <- stan_model(stan.file, verbose = TRUE)


coin.coin5.stanfit <- sampling(coin.coin5.stanmodel
                              ,data      = stan.data.lst
                              ,algorithm = "NUTS"
                              ,warmup    = warmup.count
                              ,iter      = warmup.count + sample.count
                              ,chains    = chain.count
                              ,verbose   = TRUE
                               )

traceplot(coin.coin5.stanfit) + expand_limits(y = c(0,1))
