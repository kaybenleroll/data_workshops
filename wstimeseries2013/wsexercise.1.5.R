###
### Worksheet Exercise 1.5
###

source('setup_data.R', echo = TRUE);

MA.year.ts        <- aggregate(MA.month.ts);
MA.annual.mean.ts <- MA.year.ts / 12;


layout(1:2)
plot(MA.month.ts,       ylab = "unemployed (%)")
plot(MA.annual.mean.ts, ylab = "unemployed (%)")


### We can also plot this in ggplot2
MA.month.vec       <- as.vector(MA.month.ts);
MA.annual.mean.vec <- as.vector(MA.annual.mean.ts);


qplot(1:length(MA.month.vec), MA.month.vec, geom = 'line', colour = I('red')) +
    geom_line(aes(x = -6 + (1:length(MA.annual.mean.vec)) * 12, y = MA.annual.mean.vec), colour = 'blue');
