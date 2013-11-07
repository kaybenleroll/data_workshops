###
### Worksheet Exercise 4.1
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);

AP.ts.decomp <- decompose(AP.ts);
AP.ts.decomp.random <- AP.ts.decomp$random[!is.na(AP.ts.decomp$random)]

AP.ts.decomp.random.acf <- acf(AP.ts.decomp.random, plot = FALSE);
