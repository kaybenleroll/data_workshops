library(data.table)
library(ggplot2)
library(rstan)
library(gridExtra)




generate.disease.test.data <- function(n = 1000000, prior.prob = 0.001, hit.rate = 0.99
                                     , false.alarm = 0.05) {
    infected <- rbinom(n, 1, prior.prob)

    count.infected <- length(infected[infected == 1])

    test.positive <- rep(0, n)

    test.positive[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate)
    test.positive[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm)

    return(data.table(infected = infected, test.positive = test.positive))
}


calculate.disease.test.probabilities <- function(data.dt) {
    stopifnot(is.data.table(data.dt))

    return(data.dt[test.positive == 1, .N, by = infected][, .(infected, prop = N / sum(N))])
}


generate.disease.twotest.data <- function(n = 1000000, prior.prob = 0.001
                                        , hit.rate.1 = 0.99, false.alarm.1 = 0.05
                                        , hit.rate.2 = hit.rate.1, false.alarm.2 = false.alarm.1) {

    infected <- rbinom(n, 1, prior.prob)

    count.infected <- length(infected[infected == 1])

    test.1 <- rep(0, n)

    test.1[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate.1)
    test.1[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm.1)

    test.2 <- rep(0, n)

    test.2[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate.2)
    test.2[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm.2)


    return(data.table(infected = infected, test.1 = test.1, test.2 = test.2))
}


calculate.likelihood <- function(y, theta) { return(theta^y * (1 - theta)^(1 - y)) }


calculate.data.probability <- function(data, theta) {
    N <- length(theta)

    prob.data.given.theta <- sapply(theta, function(itertheta) {
        return(exp(sum(log(calculate.likelihood(data, itertheta)))))
    })

    midpoint   <- 0.5 * (prob.data.given.theta[-1] + prob.data.given.theta[-N])
    normaliser <- sum(diff(theta) * midpoint)

    norm.prob <- prob.data.given.theta / normaliser

    return(norm.prob)
}


calculate.posterior.density <- function(prior, data, theta) {
    stopifnot(length(prior) == length(theta))

    N <- length(data)

    data.prob <- calculate.data.probability(data, theta)

    posterior <- data.prob * prior

    midpoint <- 0.5 * (posterior[-1] + posterior[-N])
    normaliser <- sum(diff(theta) * midpoint)

    posterior.norm <- posterior / normaliser

    return(posterior.norm)
}


calculate.hierarchical.posterior <- function(data, mu, theta, mu.prior, K) {

    p.y.given.theta  <- calculate.p.y.given.theta(data, mu, theta)
    p.theta.given.mu <- calculate.p.theta.given.mu(mu, theta, K)
    mu.grid          <- calculate.mu.prior(mu.prior, theta)

    posterior <- p.y.given.theta * p.theta.given.mu * mu.grid

    normaliser <- sum(posterior * diff(mu)[1] * diff(theta)[1])

    posterior.norm <- posterior / normaliser


    return(posterior.norm)
}


calculate.p.y.given.theta <- function(data, mu, theta) {
    calc.prob <- function(mu, theta) {
        prob <- calculate.data.probability(data, theta)

        return(prob)
    }

    return(outer(mu, theta, calc.prob))
}


calculate.p.theta.given.mu <- function(mu, theta, K) {
    data <- outer(mu, theta, function(x, y) { dbeta(y, x * K, (1-x) * K) })

    return(data)
}


calculate.mu.prior <- function(mu, theta) {
    data <- matrix(rep(mu, length(theta)), nrow = length(theta), byrow = TRUE)

    return(t(data))
}


generate.coin.data <- function(theta, toss.count) {
    data.dt <- data.table(theta = theta, N = toss.count)

    data.dt <- data.dt[, .(success = rbinom(N, size = N, prob = theta), trials = N), by = .I]

    data.dt[, coin.id := .I]

    return(data.dt[, .(coin.id, success, trials)])
}


generate.hierarchical.coin.data <- function(mu = 0.5, K = 20, coins = 5, total.tosses = 250) {

    toss.per.coin <- round(total.tosses / coins, 0)

    theta.vals  <- rbeta(coins, mu * K, (1 - mu) * K)

    data.dt <- data.table(theta = theta.vals, N = toss.per.coin)
    data.dt <- data.dt[, .(success = rbinom(N, size = N, prob = theta), trials = N), by = .I]

    data.dt[, coin.id := .I]

    return(data.dt)
}


generate.multiple.mint.data <- function(mu = c(0.48, 0.51, 0.47, 0.51, 0.53)
                                       ,K  = c(1000,  750,  500, 1500, 4000)
                                       ,coin.mean =  50, coin.sd = 10
                                       ,toss.mean = 250, toss.sd = 50) {

    mint.count <- length(mu)
    coin.count <- round(rnorm(mint.count, coin.mean, coin.sd), 0)

    mint.dt <- data.table(mint.id = 1:mint.count, mu, K, coin.count)

    mint.dt <- mint.dt[, {
        theta = rbeta(coin.count, mu * K, (1 - mu) * K)

        .(mu = mu, K = K, coin.count = coin.count, theta = theta)
    }, by = mint.id]

    mint.dt[, toss.count := round(rnorm(.N, toss.mean, toss.sd), 0)]
    mint.dt[, coin.id := .I]

    mint.dt <- mint.dt[, {
        success = rbinom(1, size = toss.count, prob = theta)

        .(mint.id = mint.id, mu = mu, K = K, coin.count = coin.count
         ,theta = theta, toss.count = toss.count, success = success)

    }, by = coin.id]

    return(mint.dt[, .(mint.id, mu, K, coin.count, coin.id, theta, toss.count, success)])
}
