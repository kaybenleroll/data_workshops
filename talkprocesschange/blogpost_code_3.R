source("lib.R");

set.seed(42);

### Create data for a small change, two additional months
mu <- 0.1;
K1 <- 6000;
K2 <- 7000;

x_seq <- seq(0.05, 0.15, by = 0.0001);

Beta1 <- dbeta(x_seq, mu * K1, (1 - mu) * K1);
Beta2 <- dbeta(x_seq, mu * K2, (1 - mu) * K2);

staticmu_1_plot <- qplot(x_seq, Beta1, geom = 'line', xlab = expression(theta), ylab = 'Probability Density', ylim = c(0, 150)) +
    geom_line(aes(y = Beta2), colour = 'red') +
    geom_area(aes(x = x_seq, y = pmin(Beta1, Beta2)), fill = 'grey', alpha = 0.5);


### Now suppose we add a full years worth of data
K3 <- 12000;

Beta3 <- dbeta(x_seq, mu * K3, (1 - mu) * K3);

staticmu_2_plot <- qplot(x_seq, Beta1, geom = 'line', xlab = expression(theta), ylab = 'Probability Density', ylim = c(0, 150)) +
    geom_line(aes(y = Beta3), colour = 'red') +
    geom_area(aes(x = x_seq, y = pmin(Beta1, Beta3)), fill = 'grey', alpha = 0.5);


double_plot <- arrangeGrob(staticmu_1_plot, staticmu_2_plot, ncol = 2);
ggsave(double_plot, file = "double_static_plot.png", width = 14, height = 10);


### Calculate the distance metrics for Beta1, Beta2 and Beta3
x_seq <- seq(0, 1, by = 0.0001);
mu <- 0.1;
K1 <- 6000;
K2 <- 7000;
K3 <- 12000;

Beta1 <- dbeta(x_seq, mu * K1, (1 - mu) * K1);
Beta2 <- dbeta(x_seq, mu * K2, (1 - mu) * K2);
Beta3 <- dbeta(x_seq, mu * K3, (1 - mu) * K3);


print(calculate_metrics(x_seq, Beta1, Beta1));
print(calculate_metrics(x_seq, Beta1, Beta2));
print(calculate_metrics(x_seq, Beta1, Beta3));
cat("\n\n");


### Calculate distance metrics when shifting mu
x_seq <- seq(0, 1, by = 0.0001);
mu1 <- 0.1;
mu2 <- 0.11;
K1 <- 6000;
K2 <- 7000;
K3 <- 12000;

Beta1 <- dbeta(x_seq, (mu1 * K1),                     ((1 - mu1) * K1));
Beta2 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K2 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K2 - K1))));
Beta3 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K3 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K3 - K1))));

beta_distance_011_plot <- qplot(x_seq[x_seq >= 0.075 & x_seq <= 0.125], Beta1[x_seq >= 0.075 & x_seq <= 0.125], geom = 'line') +
    geom_line(aes(y = Beta2[x_seq >= 0.075 & x_seq <= 0.125]), colour = 'red') +
    geom_line(aes(y = Beta3[x_seq >= 0.075 & x_seq <= 0.125]), colour = 'blue')
ggsave(beta_distance_011_plot, file = 'beta_distance_011_plot.png', width = 14, height = 10);


print(calculate_metrics(x_seq, Beta1, Beta1));
print(calculate_metrics(x_seq, Beta1, Beta2));
print(calculate_metrics(x_seq, Beta1, Beta3));
cat("\n\n");


### Calculate distance metrics when shifting mu
x_seq <- seq(0, 1, by = 0.0001);
mu1 <-  0.10;
mu2 <-  0.15;
K1  <-  6000;
K2  <-  7000;
K3  <- 12000;

Beta1 <- dbeta(x_seq, (mu1 * K1),                     ((1 - mu1) * K1));
Beta2 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K2 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K2 - K1))));
Beta3 <- dbeta(x_seq, (mu1 * K1) + (mu2 * (K3 - K1)), ((1 - mu1) * K1 + ((1 - mu2) * (K3 - K1))));

beta_distance_015_plot <- qplot(x_seq[x_seq >= 0.075 & x_seq <= 0.150], Beta1[x_seq >= 0.075 & x_seq <= 0.150], geom = 'line') +
    geom_line(aes(y = Beta2[x_seq >= 0.075 & x_seq <= 0.150]), colour = 'red') +
    geom_line(aes(y = Beta3[x_seq >= 0.075 & x_seq <= 0.150]), colour = 'blue')
ggsave(beta_distance_015_plot, file = 'beta_distance_015_plot.png', width = 14, height = 10);


print(calculate_metrics(x_seq, Beta1, Beta1));
print(calculate_metrics(x_seq, Beta1, Beta2));
print(calculate_metrics(x_seq, Beta1, Beta3));
cat("\n\n");


### Generate all this data using the new routine, and look at the values for different parameter changes
mu1_vec <- c(0.10, 0.10, 0.40, 0.40);
mu2_vec <- c(0.11, 0.15, 0.45, 0.50);

# Generate data for each combination of mu1, mu2 and do some transformations to create a data.table
comparison_lst     <- mapply(generate_beta_comparison_data, mu1_vec, mu2_vec, SIMPLIFY = FALSE);
comparison_data_dt <- data.table(mu1 = mu1_vec, mu2 = mu2_vec, t(sapply(comparison_lst, function(x) c(dist12 = x$dist12, dist13 = x$dist13))));

comparison_metric_plot <- qplot(sprintf("(%4.2f, %4.2f)", mu1, mu2), value, data = melt(comparison_data_dt, c("mu1", "mu2")),
                                xlab = expression(paste("(", mu[1], ", ", mu[2], ")")), ylab = 'Distance Metric') +
    facet_wrap(~ variable, scales = 'free') + expand_limits(y = 0);
ggsave(comparison_metric_plot, file = 'comparison_metric_plot.png', width = 14, height = 10);



### Calculate the metrics on the post-change data
postchange_data_dt <- stochastic_rate_count_data_dt[rate_date >= as.Date('2014-01-01'),
                                                    list(rate_date, a = cumsum(conversion_count), b = cumsum(month_count) - cumsum(conversion_count))];
postchange_plot_data_dt <- melt(postchange_data_dt[, data.table(t(calculate_postchange_metrics(init_mu = 0.1, init_K = 6000, a, b))), by = rate_date], 'rate_date');

postchange_plot <- qplot(rate_date, value, data = postchange_plot_data_dt, geom = 'line', xlab = 'Date', ylab = 'metric') +
    facet_wrap(~ variable, scale = 'free') +
    expand_limits(y = 0);
ggsave(postchange_plot, file = 'postchange_metric_plot.png', width = 14, height = 10);



### Calculate the metrics on the post-change data
highbase_postchange_data_dt <- highbase_count_data_dt[rate_date >= as.Date('2014-01-01'),
                                                     list(rate_date, a = cumsum(conversion_count), b = cumsum(month_count) - cumsum(conversion_count))];
highbase_postchange_plot_data_dt <- melt(highbase_postchange_data_dt[, data.table(t(calculate_postchange_metrics(init_mu = 0.4, init_K = 6000, a, b))), by = rate_date], 'rate_date');

highbase_postchange_plot <- qplot(rate_date, value, data = highbase_postchange_plot_data_dt, geom = 'line', xlab = 'Date', ylab = 'metric') +
    facet_wrap(~ variable, scale = 'free') +
    expand_limits(y = 0);
ggsave(highbase_postchange_plot, file = 'highbase_postchange_metric_plot.png', width = 14, height = 10);
