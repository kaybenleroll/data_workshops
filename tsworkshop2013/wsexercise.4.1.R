###
### Worksheet Exercise 4.1
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);

str(AP.ts.acf);
