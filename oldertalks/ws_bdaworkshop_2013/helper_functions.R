
generate.disease.test.data <- function(n = 1000000, prior.prob = 0.001, hit.rate = 0.99, false.alarm = 0.05) {
    infected <- rbinom(n, 1, prior.prob);

    count.infected <- length(infected[infected == 1]);

    test.positive <- rep(0, n);

    test.positive[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate);
    test.positive[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm);

    return(rbind(infected = infected, test.positive = test.positive));
}


calculate.disease.test.probabilities <- function(data) {
    infected      <- data[1, ];
    test.positive <- data[2, ];

    accuracy.prob <- length(infected[infected == 1 & test.positive == 1]) / length(infected[test.positive == 1]);

    return(accuracy.prob);
}


generate.disease.twotest.data <- function(n = 1000000, prior.prob = 0.001, hit.rate.1 = 0.99, false.alarm.1 = 0.05, hit.rate.2 = 0.99, false.alarm.2 = 0.05) {
    infected <- rbinom(n, 1, prior.prob);

    count.infected <- length(infected[infected == 1]);

    test.1 <- rep(0, n);

    test.1[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate.1);
    test.1[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm.1);

    test.2 <- rep(0, n);

    test.2[infected == 1] <- rbinom(length(infected[infected == 1]), 1, hit.rate.2);
    test.2[infected == 0] <- rbinom(length(infected[infected == 0]), 1, false.alarm.2);


    return(rbind(infected = infected, test.1 = test.1, test.2 = test.2));
}


do.metropolis.island.sampling <- function(island.distrib, proposal.prob = 0.5, init.island = 1, sample.count = 1000, burn.in = 100) {

    i <- 0;
    island.count <- length(island.distrib);

    total.count <- sample.count + burn.in;
    sample.data <- rep(0, total.count);

    sample.data[1] <- init.island;


    for(i in 1:(total.count-1)) {
        proposal <- sample.data[i] + ifelse(runif(1, 0, 1) > proposal.prob, -1, 1);

        if(proposal == 0)           proposal <- island.count;
        if(proposal > island.count) proposal <- 1;

        if(island.distrib[proposal] > island.distrib[sample.data[i]]) {
            new.island <- proposal;
        } else {
            random.number <- runif(1, 0, 1);

            if(random.number < (island.distrib[proposal] / island.distrib[sample.data[i]])) {
                new.island <- proposal;
            } else {
                new.island <- sample.data[i];
            }
        }

        sample.data[i+1] <- new.island;
    }

    return(sample.data[-(1:burn.in)]);
}


calculate.likelihood <- function(y, theta) { return(theta^y * (1 - theta)^(1 - y)) }

calculate.data.probability <- function(data, theta) {
    N <- length(theta);

    prob.data.given.theta <- sapply(theta, function(itertheta) { return(prod(calculate.likelihood(data, itertheta))) })

    midpoint   <- 0.5 * (prob.data.given.theta[-1] + prob.data.given.theta[-N])
    normaliser <- sum(diff(theta) * midpoint);

    norm.prob <- prob.data.given.theta / normaliser;

    return(norm.prob);
}


calculate.posterior.density <- function(prior, data, theta) {
    stopifnot(length(prior) == length(theta));

    N <- length(data);

    data.prob <- calculate.data.probability(data, theta);

    posterior <- data.prob * prior;

    midpoint <- 0.5 * (posterior[-1] + posterior[-N]);
    normaliser <- sum(diff(theta) * midpoint);

    posterior.norm <- posterior / normaliser;

    return(posterior.norm);
}


calculate.hierarchical.posterior <- function(data, mu, theta, mu.prior, K) {

    p.y.given.theta  <- calculate.p.y.given.theta(data, mu, theta);
    p.theta.given.mu <- calculate.p.theta.given.mu(mu, theta, K);
    mu.grid          <- calculate.mu.prior(mu.prior, theta);

    posterior <- p.y.given.theta * p.theta.given.mu * mu.grid;

    normaliser <- sum(posterior * diff(mu)[1] * diff(theta)[1]);

    posterior.norm <- posterior / normaliser;


    return(posterior.norm);
}


calculate.p.y.given.theta <- function(data, mu, theta) {
    calc.prob <- function(mu, theta) {
        prob <- calculate.data.probability(data, theta);

        return(prob);
    }

    return(outer(mu, theta, calc.prob));
}


calculate.p.theta.given.mu <- function(mu, theta, K) {
    data <- outer(mu, theta, function(x, y) { dbeta(y, x * K, (1-x) * K) });

    return(data);
}


calculate.mu.prior <- function(mu, theta) {
    data <- matrix(rep(mu, length(theta)), nrow = length(theta), byrow = TRUE);

    return(t(data));
}


generate.hierarchical.coin.data <- function(mu = 0.5, K = 20, coins = 5, total.tosses = 250) {

    toss.per.coin <- round(total.tosses / coins, 0);

    theta.vals  <- rbeta(coins, mu * K, (1 - mu) * K);

    theta    <- unlist(lapply(1:coins, function(idx) rep(theta.vals[idx], toss.per.coin)));
    coin     <- unlist(lapply(1:coins, function(idx) rep(idx, toss.per.coin)));
    cointoss <- unlist(lapply(theta.vals, function(coinmu) rbinom(toss.per.coin, 1, coinmu)))

    return(rbind(theta, coin, cointoss));
}
