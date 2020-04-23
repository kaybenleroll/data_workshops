

generate.cointoss.data <- function(mu, K, coin.count, toss.min, toss.max) {
    stopifnot(length(mu) == length(K));
    stopifnot(length(mu) == length(coin.count));

    mintdata.dt <- data.table(mintid = 1:length(mu), mu = mu, K = K, coin.count = coin.count);

    coindata.dt <- mintdata.dt[, list(theta = rbeta(.SD$coin.count, .SD$mu * .SD$K, (1 - .SD$mu) * .SD$K)), by = mintid];
    coindata.dt[, coinid := 1:length(theta)];

    tossdata.dt <- coindata.dt[, list(outcome = rbinom(round(runif(1, toss.min, toss.max), 0), 1, theta)), by = coinid];

    cointoss.dt <- merge(coindata.dt[, list(mintid, coinid)], tossdata.dt[, list(coinid, outcome)], by = 'coinid');

    return(cointoss.dt);
}
