###
### Worksheet Exercise 4.1
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);

plot(AP.ts.acf);
