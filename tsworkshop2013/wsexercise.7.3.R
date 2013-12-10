###
### Worksheet Exercise 7.1
###

source('setup_data.R', echo = TRUE);

set.seed(42);

arma22.ts <- arima.sim(list(order = c(2, 0, 2), ar = c(0.2, -0.5), ma = c(-0.1, 0.3)), n = 1000)

arma22.ts.pacf <- pacf(arma11.ts);


layout(1:2);
plot(arma22.ts);
plot(arma22.ts.pacf);


arma22.fit <- arima(arma22.ts, order = c(2, 0, 2));
print(arma22.fit);
