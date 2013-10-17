###
### Worksheet Exercise 1.4
###

source('setup_data.R', echo = TRUE);


### We are going to aggregate over the years, and extract the cycles
MA.year.ts  <- aggregate(MA.month.ts);
MA.cycle.ts <- cycle(MA.month.ts);


### We are going to stack the two plots together
layout(1:2)
plot(MA.year.ts)
boxplot(MA.month.ts ~ MA.cycle.ts)


### Create a plot in ggplot2

#plot1 <- qplot(start(MA.year.ts)[1]:end(MA.year.ts)[1], as.vector(MA.year.ts), geom = 'line', xlab = 'Year', ylab = 'Yearly Aggregates')
#plot2 <- qplot(MA.cycle.ts, MA.month.ts, data = data.frame(cycle = as.factor(MA.cycle.ts), MA = as.vector(MA.month.ts)), geom = 'boxplot', xlab = 'Month', ylab = 'Passengers');

#grid.arrange(plot1, plot2);
