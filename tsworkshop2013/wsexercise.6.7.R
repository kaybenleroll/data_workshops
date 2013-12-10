###
### Worksheet Exercise 6.7
###

source('setup_data.R', echo = TRUE);

set.seed(42);

ar1.fit <- ar(ar1.ts);
print(ar1.fit);

ar2a.fit <- ar(ar2a.ts);
print(ar2a.fit);
