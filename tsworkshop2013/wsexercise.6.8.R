###
### Worksheet Exercise 6.8
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ma1.ts <- arima.sim(list(order = c(0, 0, 1), ma = 0.5), n = 1000, start.innov = 1, n.start = 1);

plot(ma1.ts);
