###
### Worksheet Exercise 4.3
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);

plot(AP.ts.acf, ylim = c(-1, 1));
