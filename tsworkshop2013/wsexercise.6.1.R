###
### Worksheet Exercise 6.1
###

source('setup_data.R', echo = TRUE);

set.seed(42);

x <- rnorm(99, 0, 1);

whitenoise.ts     <- ts(c(1, x));
whitenoise.ts.acf <- acf(whitenoise.ts, plot = FALSE);

layout(1:2);
plot(whitenoise.ts,     ylim = c(-4, 4));
plot(whitenoise.ts.acf, ylim = c(-1, 1));
