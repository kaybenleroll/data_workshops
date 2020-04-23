library(data.table);
library(reshape2);
library(ggplot2);
library(xts);
library(gridExtra);


generate_process_rates <- function(mu0 = 0.10, sd0 = 0.03, mu1 = 0.15, sd1 = 0.03,
                                   start_date  = as.Date("2010-01-01"),
                                   end_date    = as.Date("2015-03-01"),
                                   change_date = as.Date("2014-01-01")) {

    month_vector <- as.yearmon(seq(start_date, end_date, by = "month"));
    switch_month <- as.yearmon(change_date);

    switch_idx <- match(switch_month, month_vector);

    pre_rate  <- rnorm(switch_idx - 1, mu0, sd0);
    post_rate <- rnorm(length(month_vector) - switch_idx + 1, mu1, sd1);

    rate_dt <- data.table(rate_date = as.Date(month_vector), underlying_rate = c(pre_rate, post_rate));

    return(rate_dt)
}


generate_counts <- function(rate_dt, month_count) {
    rate_dt <- data.table(rate_dt, month_count = month_count);

    rate_dt[, conversion_count := mapply(rbinom, n = 1, month_count, underlying_rate)];
    rate_dt[, conversion_rate  := conversion_count / month_count];

    return(rate_dt);
}


generate_beta_plot_data <- function(a, b) {
    theta     <- seq(0, 1, by = 0.0001);
    prob_dens <- dbeta(theta, a, b);

    return(data.table(theta = theta, prob_dens = prob_dens));
}


generate_yearly_data <- function(rate_dt) {
    year_dt <- rate_dt[, list(a = sum(conversion_count), b = sum(month_count - conversion_count)), by = list(data_year = format(rate_date, '%Y'))];
    year_dt[, c("cum_a", "cum_b") := list(cumsum(a) + 1, cumsum(b) + 1)];

    distrib_dt <- year_dt[, generate_beta_plot_data(cum_a, cum_b), by = data_year];

    return(distrib_dt);
}


generate_beta_comparison_data <- function(mu1 = 0.10, mu2 = 0.15, K1 = 6000, K2 = 7000, K3 = 12000) {
    x_seq <- seq(0, 1, by = 0.0001);

    Beta1 <- dbeta(x_seq, (mu1 * K1),                     ((1 - mu1) * K1));
    Beta2 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K2 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K2 - K1))));
    Beta3 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K3 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K3 - K1))));

    dist12 <- calculate_metrics(x_seq, Beta1, Beta2);
    dist13 <- calculate_metrics(x_seq, Beta1, Beta3);

    return(list(x_seq  = x_seq,
                Beta1  = Beta1,
                Beta2  = Beta2,
                Beta3  = Beta3,
                dist12 = dist12,
                dist13 = dist13));
}


common_area_metric <- function(x, P, Q) {
    stopifnot(length(P) == length(Q));
    stopifnot(length(x) == length(P));

    N  <- length(x);
    dx <- diff(x);

    return(1 - sum(pmin(P, Q)[2:N] * dx));
}


hellinger_metric <- function(x, P, Q) {
    stopifnot(length(P) == length(Q));
    stopifnot(length(x) == length(P));

    N  <- length(x);
    dx <- diff(x);

    return(1 - sum(sqrt(P * Q)[2:N] * dx));
}


kl_metric <- function(x, P, Q) {
    stopifnot(length(P) == length(Q));
    stopifnot(length(x) == length(P));

    N  <- length(x);
    dx <- diff(x);
    dx <- c(dx[1], dx);

    ### Need to select out the non-zero indices for both P and Q
    idx <- intersect(which(P > 0),  which(Q > 0));

    return(sum((P * log(P/Q) * dx)[idx]));
}

beta_hellinger_metric <- function(alpha1, beta1, alpha2, beta2) {
    B1 <- beta(0.5 * (alpha1 + alpha2), 0.5 * (beta1 + beta2));
    B2 <- beta(alpha1, beta1);
    B3 <- beta(alpha2, beta2);

    return(sqrt(1 - (B1 / sqrt(B2 * B3))));
}


calculate_metrics <- function(x, P, Q) {
    stopifnot(length(P) == length(Q));
    stopifnot(length(x) == length(P));

    commonarea <- common_area_metric(x, P, Q);
    hellinger  <- hellinger_metric(x, P, Q);
    kl         <- kl_metric(x, P, Q);

    return(c(commonarea = commonarea, hellinger = hellinger, kl = kl));
}


calculate_postchange_metrics <- function(init_mu, init_K, a, b) {
    x_seq <- seq(0, 1, by = 0.0001);

    Beta1 <- dbeta(x_seq, (init_mu * init_K),     ((1 - init_mu) * init_K));
    Beta2 <- dbeta(x_seq, (init_mu * init_K) + a, ((1 - init_mu) * init_K + b));

    return(calculate_metrics(x_seq, Beta1, Beta2));
}
