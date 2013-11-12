###
### Worksheet Exercise 6.1
###

source('setup_data.R', echo = TRUE);

set.seed(42);

x <- rnorm(99, 0, 1);

randomwalk.ts     <- ts(cumsum(c(1, x)));
randomwalk.ts.acf <- acf(randomwalk.ts, plot = FALSE);

layout(1:2);
plot(randomwalk.ts,     ylim = c(-10, 10));
plot(randomwalk.ts.acf, ylim = c( -1,  1));
