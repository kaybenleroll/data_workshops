###
### Worksheet Exercise 7.2
###

source('setup_data.R', echo = TRUE);

set.seed(42);

arma11.fit <- arima(arma11.ts, order = c(1, 0, 1));

print(arma11.fit);
