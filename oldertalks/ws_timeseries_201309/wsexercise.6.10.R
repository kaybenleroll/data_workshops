###
### Worksheet Exercise 6.10
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ma2b.ts <- arima.sim(list(order = c(0, 0, 2), ma = c(0.5, 0.5)), n = 1000, start.innov = c(1, 1), n.start = 2);

plot(ma2b.ts);
