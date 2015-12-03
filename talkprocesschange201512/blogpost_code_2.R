source("lib.R");

set.seed(42);


#### Run code for set conversion rates
stochastic_rate_rate_data_dt  <- generate_process_rates(mu0 = 0.10, mu1 = 0.15, sd0 = 0.02, sd1 = 0.02);
stochastic_rate_count_data_dt <- generate_counts(stochastic_rate_rate_data_dt, month_count = rpois(dim(stochastic_rate_rate_data_dt)[1], 500));

plot_dt <- melt(stochastic_rate_count_data_dt[, list(rate_date = as.Date(rate_date), underlying_rate, conversion_rate)],
                id.vars = 'rate_date', variable.name = 'rate_type');

stochastic_rate_rate_plot <- qplot(rate_date, value, data = plot_dt, geom = 'line', ylim = c(0, 0.2), colour = rate_type,
                                   xlab = 'Date', ylab = 'Stochastic Conversion Rate');

ggsave(stochastic_rate_rate_plot, file = 'stochastic_rate_rate.png', width = 14, height = 10);


stochastic_rate_yearly_data_dt <- generate_yearly_data(stochastic_rate_count_data_dt);

stochastic_rate_yearly_plot <- qplot(theta, prob_dens, data = stochastic_rate_yearly_data_dt, geom = 'line',
                                     colour = data_year, xlim = c(0.05, 0.15), xlab = expression(theta), ylab = "Probability Density");

ggsave(stochastic_rate_yearly_plot, file = 'stochastic_rate_yearly.png', width = 14, height = 10);



### Calculate the empirical conversion rate
stochastic_empirical_rate_dt <- stochastic_rate_count_data_dt[, list(conversion = sum(conversion_count),
                                                                     month      = sum(month_count),
                                                                     rate       = sum(conversion_count) / sum(month_count)),
                                                              by = list(year = as.numeric(format(rate_date, "%Y")))];

stochastic_empirical_rate_plot <- qplot(year, rate, data = stochastic_empirical_rate_dt, geom = 'line', ylim = c(0, 0.2));
ggsave(stochastic_empirical_rate_plot, file = 'stochastic_empirical_rate.png', width = 14, height = 10);



stochastic_rate_count_data_dt[rate_date < as.Date('2014-01-01'), list(converted = sum(conversion_count),
                                                                      calls     = sum(month_count),
                                                                      rate      = sum(conversion_count) / sum(month_count))];

stochastic_rate_count_data_dt[rate_date < as.Date('2014-01-01'),
                              list(converted = sum(conversion_count),
                                   calls     = sum(month_count),
                                   rate      = sum(conversion_count) / sum(month_count)),
                              by = list(year = as.numeric(format(rate_date, "%Y")))]



### We now visualise the prior plot
x_seq <- seq(0.075, 0.125, by = 0.0001);

mu <- 0.0997;
K  <- 6000;

prechange_prior_plot <- qplot(x_seq, dbeta(x_seq, (mu * K), (1 - mu) * K), geom = 'line', xlab = expression(theta), ylab = 'Probability Density');

ggsave(prechange_prior_plot, file = 'prechange_prior_plot.png', height = 14, width = 10);


### Run the Bayesian analysis with the new prior with K = 6000 for the first six months of 2014
sixmonths_data_dt <- stochastic_rate_count_data_dt[rate_date >= as.Date('2014-01-01') & rate_date <= as.Date('2014-06-30'),
                                                   list(rate_date, a = cumsum(conversion_count), b = cumsum(month_count) - cumsum(conversion_count))];

sixmonths_newparams_dt <- sixmonths_data_dt[, list(rate_date, new_a = (mu * K) + a, new_b = ((1 - mu) * K) + b)];
sixmonths_plotdata_dt  <- sixmonths_newparams_dt[, generate_beta_plot_data(new_a, new_b), by = rate_date];

# Create a character column for the date to help with plotting
sixmonths_plotdata_dt[, plotdate := format(rate_date, '%Y%m%d')];

sixmonths_plot <- qplot(theta, prob_dens, data = sixmonths_plotdata_dt[theta >= 0.05 & theta <= 0.15], geom = 'line',
                        colour = plotdate, xlab = expression(theta), ylab = 'Probability Density');
ggsave(sixmonths_plot, file = 'sixmonths_posterior.png', width = 14, height = 10);




### Investigate the effects of what happens when the signal moves from 0.4 to 0.45
highbase_rate_data_dt  <- generate_process_rates(mu0 = 0.40, mu1 = 0.45, sd0 = 0.08, sd1 = 0.08);
highbase_count_data_dt <- generate_counts(highbase_rate_data_dt, month_count = rpois(dim(highbase_rate_data_dt)[1], 500));

plot_dt <- melt(highbase_count_data_dt[, list(rate_date = as.Date(rate_date), underlying_rate, conversion_rate)],
                id.vars = 'rate_date', variable.name = 'rate_type');

highbase_rate_plot <- qplot(rate_date, value, data = plot_dt, geom = 'line', ylim = c(0, 0.8), colour = rate_type,
                                   xlab = 'Date', ylab = 'Stochastic Conversion Rate');

ggsave(highbase_rate_plot, file = 'highbase_rate.png', width = 14, height = 10);



high_mu <- highbase_count_data_dt[rate_date < as.Date('2014-01-01'), sum(conversion_count) / sum(month_count)];
high_K  <- 6000;

highbase_data_dt <- highbase_count_data_dt[rate_date >= as.Date('2014-01-01') & rate_date <= as.Date('2014-06-30'),
                                           list(rate_date, a = cumsum(conversion_count), b = cumsum(month_count) - cumsum(conversion_count))];

highbase_newparams_dt <- highbase_data_dt[, list(rate_date, new_a = (high_mu * high_K) + a, new_b = ((1 - high_mu) * high_K) + b)];
highbase_plotdata_dt  <- highbase_newparams_dt[, generate_beta_plot_data(new_a, new_b), by = rate_date];

highbase_plotdata_dt[, plotdate := format(rate_date, '%Y%m%d')];
highbase_plot <- qplot(theta, prob_dens, data = highbase_plotdata_dt[theta >= 0.35 & theta <= 0.50], geom = 'line',
                       colour = plotdate, xlab = expression(theta), ylab = 'Probability Density');

ggsave(highbase_plot, file = 'highbase_posterior.png', width = 14, height = 10);



### Create plot for area under the two curves
theta_seq <- seq(0, 1, by = 0.0001);

P <- dbeta(theta_seq, 50, 50);
Q <- dbeta(theta_seq, 40, 60);

areaunder_plot <- qplot(theta_seq, P, geom = 'line', xlab = expression(theta), ylab = 'Probability Density') +
    geom_line(aes(y = Q), colour = 'red') +
    geom_area(aes(x = theta_seq, y = pmin(P, Q)), fill = 'grey', alpha = 0.5);

ggsave(areaunder_plot, file = 'areaunder_plot.png', width = 14, height = 10);


### Now we have a look at the common area metric for the months after the change point

base_dens <- highbase_plotdata_dt[plotdate == "20140101", prob_dens];
highbase_commonarea_dt <- highbase_plotdata_dt[, list(common_area = common_area_metric(theta, prob_dens, base_dens)), by = rate_date]

commonarea_metric_plot <- qplot(rate_date, common_area, data = highbase_commonarea_dt, geom = 'line', ylim = c(0, 1.05), xlab = "Date", ylab = "Common Area");

ggsave(commonarea_metric_plot, file = 'commonarea_metric_plot.png', width = 14, height = 10);
