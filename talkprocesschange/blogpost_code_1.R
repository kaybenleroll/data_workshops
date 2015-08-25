source("lib.R");

set.seed(42);


plot_rate_dt <- generate_process_rates(mu0 = 0.10, sd0 = 0.02, mu1 = 0.15, sd1 = 0.03);

rate_plot <- qplot(rate_date, underlying_rate, data = plot_rate_dt, geom = 'line', ylim = c(0, 0.2), xlab = 'Date', ylab = 'Conversion Rate');
ggsave(rate_plot, file = 'conversion_rate_plot.png', width = 14, height = 10);


#### Run code for set conversion rates

set_conversion_rate_data_dt  <- generate_process_rates(mu0 = 0.10, mu1 = 0.15, sd0 = 0, sd1 = 0);
set_conversion_count_data_dt <- generate_counts(set_conversion_rate_data_dt, month_count = 500);

plot_dt <- melt(set_conversion_count_data_dt[, list(rate_date = as.Date(rate_date), underlying_rate, conversion_rate)],
                id.vars = 'rate_date', variable.name = 'rate_type');

set_conversion_rate_plot <- qplot(rate_date, value, data = plot_dt, geom = 'line', ylim = c(0, 0.2), colour = rate_type,
                                  xlab = 'Date', ylab = 'Set Conversion Rate');

ggsave(set_conversion_rate_plot, file = 'set_conversion_rate.png', width = 14, height = 10);


set_conversion_yearly_data_dt <- generate_yearly_data(set_conversion_count_data_dt);

set_conversion_yearly_plot <- qplot(theta, prob.dens, data = set_conversion_yearly_data_dt, geom = 'line',
                                    colour = data_year, xlim = c(0.05, 0.15), xlab = expression(theta), ylab = "Probability Density");

ggsave(set_conversion_yearly_plot, file = 'set_conversion_yearly.png', width = 14, height = 10);



#### Run code for variable month count data

month_count_rate_data_dt  <- generate_process_rates(mu0 = 0.10, mu1 = 0.15, sd0 = 0, sd1 = 0);
month_count_count_data_dt <- generate_counts(month_count_rate_data_dt, month_count = rpois(dim(month_count_rate_data_dt)[1], 500));

plot_dt <- melt(month_count_count_data_dt[, list(rate_date = as.Date(rate_date), underlying_rate, conversion_rate)],
                id.vars = 'rate_date', variable.name = 'rate_type');

month_count_rate_plot <- qplot(rate_date, value, data = plot_dt, geom = 'line', ylim = c(0, 0.2), colour = rate_type,
                               xlab = 'Date', ylab = 'Set Conversion Rate');

ggsave(month_count_rate_plot, file = 'month_count_rate.png', width = 14, height = 10);


month_count_yearly_data_dt <- generate_yearly_data(month_count_count_data_dt);

month_count_yearly_plot <- qplot(theta, prob.dens, data = month_count_yearly_data_dt, geom = 'line',
                                    colour = data_year, xlim = c(0.05, 0.15), xlab = expression(theta), ylab = "Probability Density");

ggsave(month_count_yearly_plot, file = 'month_count_yearly.png', width = 14, height = 10);


### Compare the constant month rate with the variable month rate
compare_dt <- rbind(set_conversion_count_data_dt[, list(rate_date, month_type = 'fixed', conversion_rate)],
                    month_count_count_data_dt   [, list(rate_date, month_type = 'variable', conversion_rate)]);

compare_plot <- qplot(rate_date, conversion_rate, data = compare_dt, geom = 'line', ylim = c(0, 0.2), colour = month_type,
                      xlab = 'Date', ylab = 'Conversion Rate')
