###
### Worksheet Exercise 6.9
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ma2a.ts <- arima.sim(list(order = c(0, 0, 2), ma = c(1, -0.25)), n = 1000, start.innov = c(1, 1), n.start = 2);

plot(ma2a.ts);
