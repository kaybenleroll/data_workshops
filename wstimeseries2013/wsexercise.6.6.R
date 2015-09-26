###
### Worksheet Exercise 6.6
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ar2b.ts <- arima.sim(list(order = c(2, 0, 0), ar = c(0.5, 0.5)), n = 1000, start.innov = c(1, 1), n.start = 2);

plot(ar2b.ts);
