###
### Worksheet Exercise 1.3
###

source('setup_data.R', echo = TRUE);


### We are going to aggregate over the years, and extract the cycles
AP.year  <- aggregate(AP);
AP.cycle <- cycle(AP);


### We are going to stack the two plots together
layout(1:2)
plot(AP.year)
boxplot(AP ~ AP.cycle)


### Create a plot in ggplot2

#plot1 <- qplot(start(AP.year)[1]:end(AP.year)[1], as.vector(AP.year), geom = 'line', xlab = 'Year', ylab = 'Yearly Aggregates')
#plot2 <- qplot(cycle, AP, data = data.frame(cycle = as.factor(AP.cycle), AP = as.vector(AP)), geom = 'boxplot', xlab = 'Month', ylab = 'Passengers');

#grid.arrange(plot1, plot2);
