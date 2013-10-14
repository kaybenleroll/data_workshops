###
### Worksheet Exercise 1.4
###

source('setup_data.R', echo = TRUE);


### We are going to aggregate over the years, and extract the cycles
MA.year  <- aggregate(Maine.month.ts);
MA.cycle <- cycle(Maine.month.ts);


### We are going to stack the two plots together
layout(1:2)
plot(MA.year)
boxplot(MA.month.ts ~ MA.cycle)


### Create a plot in ggplot2

#plot1 <- qplot(start(MA.year)[1]:end(MA.year)[1], as.vector(MA.year), geom = 'line', xlab = 'Year', ylab = 'Yearly Aggregates')
#plot2 <- qplot(cycle, MA, data = data.frame(cycle = as.factor(MA.cycle), MA = as.vector(MA)), geom = 'boxplot', xlab = 'Month', ylab = 'Passengers');

#grid.arrange(plot1, plot2);
