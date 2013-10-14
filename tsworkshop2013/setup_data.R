###
### Worksheet Data Setup
###

library(ggplot2);
library(gridExtra);


data(AirPassengers);

AP          <- AirPassengers;
MA.month    <- read.table('Maine.dat', header = TRUE);
MA.month.ts <- ts(MA.month$unemploy, start = c(1996, 1), freq = 12)
