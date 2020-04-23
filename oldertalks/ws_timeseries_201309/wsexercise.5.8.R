###
### Worksheet Exercise 5.8
###

source('setup_data.R', echo = TRUE);


AP.ts.mult.hw.predict <- predict(AP.ts.mult.hw, n.ahead = 4 * 12);

ts.plot(AP.ts, AP.ts.mult.hw.predict, lty = 1:2);
