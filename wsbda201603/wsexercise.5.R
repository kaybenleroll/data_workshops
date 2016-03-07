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
