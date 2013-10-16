###
### Worksheet Exercise 1.5
###

source('setup_data.R', echo = TRUE);

elec.ts <- ts(CBE.df$elec, start = 1958, freq = 12);
AP.elec.ts <- ts.intersect(AP.ts, elec.ts);
AP.elec.cor <- cor(AP.elec.ts);

str(AP.elec.cor);


qplot(Var1, value, data = melt(scale(AP.elec.ts)), geom = 'line', colour = Var2);
