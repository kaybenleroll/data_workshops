###
### Worksheet Data Setup
###

library(ggplot2);
library(gridExtra);


data(AirPassengers);

AP.ts       <- AirPassengers;
MA.month.df <- read.table('Maine.dat', header = TRUE);
MA.month.ts <- ts(MA.month.df$unemploy, start = c(1996, 1), freq = 12)

CBE.df <- read.table('cbe.dat', header = TRUE);
