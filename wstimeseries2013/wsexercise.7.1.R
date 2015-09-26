###
### Worksheet Exercise 7.1
###

source('setup_data.R', echo = TRUE);

set.seed(42);

arma11.ts <- arima.sim(list(order = c(1, 0, 1), ar = -0.6, ma = 0.5), n = 1000)

arma11.ts.pacf <- pacf(arma11.ts);


layout(1:2);
plot(arma11.ts);
plot(arma11.ts.pacf);
