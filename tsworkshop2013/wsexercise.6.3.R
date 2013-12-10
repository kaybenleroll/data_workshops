###
### Worksheet Exercise 6.3
###

source('setup_data.R', echo = TRUE);

set.seed(42);

drift.amount <- 0.05;
x            <- rnorm(99, 0, 1) + drift.amount;

randomwalk.ts     <- ts(cumsum(c(1, x)));
randomwalk.ts.acf <- acf(randomwalk.ts, plot = FALSE);

layout(1:2);
plot(randomwalk.ts,     ylim = c(-13, 13));
plot(randomwalk.ts.acf, ylim = c( -1,  1));
