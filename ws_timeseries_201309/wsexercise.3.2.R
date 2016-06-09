###
### Worksheet Exercise 3.2
###

source('setup_data.R', echo = TRUE);

beer.ts <- ts(CBE.df$beer, start = 1958, freq = 12);
choc.ts <- ts(CBE.df$choc, start = 1958, freq = 12);
elec.ts <- ts(CBE.df$elec, start = 1958, freq = 12);

beer.ts.decomp <- decompose(beer.ts);
choc.ts.decomp <- decompose(choc.ts);
elec.ts.decomp <- decompose(elec.ts);

cbe.ts <- cbind(beer.ts, choc.ts, elec.ts);

plot(cbe.ts);

plot(beer.ts.decomp);
plot(choc.ts.decomp);
plot(elec.ts.decomp);
