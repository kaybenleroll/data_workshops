###
### Worksheet Exercise 6.11
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ma1.fit  <- arima(ma1.ts,  order = c(0, 0, 1));
print(ma1.fit);

ma2a.fit <- arima(ma2a.ts, order = c(0, 0, 2));
print(ma2a.fit);

ma2b.fit <- arima(ma2b.ts, order = c(0, 0, 2));
print(ma2b.fit);
