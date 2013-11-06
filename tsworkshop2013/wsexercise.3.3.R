###
### Worksheet Exercise 3.3
###

source('setup_data.R', echo = TRUE);

AP.ts.mult.decomp <- decompose(AP.ts, type = 'multiplicative');

plot(AP.ts.mult.decomp);
