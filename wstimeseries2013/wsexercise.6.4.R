###
### Worksheet Exercise 6.4
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ar1.ts <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = 1000, start.innov = 1, n.start = 1);

plot(ar1.ts);
