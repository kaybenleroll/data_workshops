
source("setup.R");




bootstrap.calc <- function(price.returns, indexes, use.count = 120) {
    use.indexes <- indexes[1:use.count];

    optimal.weights <- portfolio.optim(price.returns[use.indexes, ], riskless = TRUE, shorts = TRUE, rf = 0.01)$pw;

    return(optimal.weights);
}


portfolio.boot <- boot(equity.returns, statistic = bootstrap.calc, R = 1000, use.count = 120);
