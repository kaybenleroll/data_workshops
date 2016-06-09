###
### Worksheet Exercise 6.5
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ar2a.ts <- arima.sim(list(order = c(2, 0, 0), ar = c(1, -0.25)), n = 1000, start.innov = c(1, 1), n.start = 2);

plot(ar2a.ts);
