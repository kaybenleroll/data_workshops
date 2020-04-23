###
### Worksheet Exercise 1.6
###

source('setup_data.R', echo = TRUE);


MA.02.ts <- window(MA.month.ts, start = c(1996, 2), freq = TRUE);
MA.08.ts <- window(MA.month.ts, start = c(1996, 8), freq = TRUE);

head(MA.02.ts); tail(MA.02.ts);

str(MA.02.ts);


Feb.ratio <- mean(MA.02.ts) / mean(MA.month.ts);
Feb.ratio

Aug.ratio <- mean(MA.08.ts) / mean(MA.month.ts);
Aug.ratio
