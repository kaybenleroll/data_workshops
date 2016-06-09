###
### Worksheet Exercise 3.1
###

source('setup_data.R', echo = TRUE);

AP.ts.decomp <- decompose(AP.ts);

str(AP.ts.decomp);

plot(AP.ts.decomp);
