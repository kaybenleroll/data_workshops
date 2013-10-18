###
### Worksheet Exercise 2.4
###

source('setup_data.R', echo = TRUE);

elec.ts     <- ts(CBE.df$elec, start = 1958, freq = 12);
AP.elec.ts  <- ts.intersect(AP.ts, elec.ts);


AP.elec.cor <- cor(AP.elec.ts);

str(AP.elec.cor);

### Show the scaled plot
qplot(Var1, value, data = melt(scale(AP.elec.ts)), geom = 'line', colour = Var2);
