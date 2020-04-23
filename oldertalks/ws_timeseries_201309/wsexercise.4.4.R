###
### Worksheet Exercise 4.4
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);

AP.ts.decomp        <- decompose(AP.ts);
AP.ts.decomp.random <- AP.ts.decomp$random[!is.na(AP.ts.decomp$random)]

AP.ts.decomp.random.acf <- acf(AP.ts.decomp.random, plot = FALSE);

layout(1:2);
plot(AP.ts.acf,               ylim = c(-1, 1));
plot(AP.ts.decomp.random.acf, ylim = c(-1, 1))
