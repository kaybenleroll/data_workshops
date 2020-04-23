###
### Worksheet Exercise 2.2
###

source('setup_data.R', echo = TRUE);


beer.ts <- ts(CBE.df$beer, start = 1958, freq = 12);
choc.ts <- ts(CBE.df$choc, start = 1958, freq = 12);
elec.ts <- ts(CBE.df$elec, start = 1958, freq = 12);

plot(cbind(beer.ts, choc.ts, elec.ts));
