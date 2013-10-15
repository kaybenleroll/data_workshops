###
### Worksheet Exercise 1.5
###

source('setup_data.R', echo = TRUE);

AP.elec.cor <- cor(AP.elec.ts);

str(AP.elec.cor);


qplot(Var1, value, data = melt(scale(AP.elec.ts)), geom = 'line', colour = Var2);
