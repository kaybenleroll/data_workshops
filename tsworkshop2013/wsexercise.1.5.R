###
### Worksheet Exercise 1.5
###

source('setup_data.R', echo = TRUE);

MA.year        <- aggregate(MA.month.ts);
MA.annual.mean <- MA.year / 12;


layout(1:2)
plot(MA.month.ts,    ylab = "unemployed (%)")
plot(MA.annual.mean, ylab = "unemployed (%)")

### We can also plot this in ggplot2
