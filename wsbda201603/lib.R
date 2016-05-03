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


generate_trial_data <- function(theta, trial_count) {
    data_dt <- data.table(theta = theta, N = trial_count)

    data_dt <- data_dt[, .(success = rbinom(N, size = N, prob = theta), trials = N), by = .I]

    data_dt[, test_id := .I]

    return(data_dt[, .(test_id, success, trials)])
}


generate_hierarchical_binomial_data <- function(mu = 0.5, K = 20, tests = 5, total_trials = 250) {

    trials_per_test <- round(total_trials / tests, 0)

    theta_vals  <- rbeta(tests, mu * K, (1 - mu) * K)

    data_dt <- data.table(theta = theta_vals, N = trials_per_test)
    data_dt <- data_dt[, .(success = rbinom(N, size = N, prob = theta), trials = N), by = .I]

    data_dt[, test_id := .I]

    return(data_dt)
}


generate_multiple_hier_trial_data <- function(mu = c(0.18, 0.11, 0.13, 0.16, 0.15)
                                             ,K  = c( 200,  250,  150,  200,  400)
                                             ,cat_mean   =  50, cat_sd   = 10
                                             ,trial_mean = 250, trial_sd = 50) {

    cat_num   <- length(mu)
    cat_count <- round(rnorm(cat_num, cat_mean, cat_sd), 0)

    binomdata_dt <- data.table(cat_id = 1:cat_num, mu, K, cat_count)

    binomdata_dt <- binomdata_dt[, {
        theta = rbeta(cat_count, mu * K, (1 - mu) * K)

        .(mu = mu, K = K, cat_count = cat_count, theta = theta)
    }, by = cat_id]

    binomdata_dt[, trial_count := round(rnorm(.N, trial_mean, trial_sd), 0)]

    binomdata_dt <- binomdata_dt[, {
        success = rbinom(1, size = trial_count, prob = theta)

        .(mu = mu, K = K, cat_count = cat_count, theta = theta
         ,trial_count = trial_count, success = success)

    }, by = cat_id]

    binomdata_dt[, trial_id := .I]

    return(binomdata_dt[, .(cat_id, mu, K, cat_count, trial_id, theta, trial_count, success)])
}
