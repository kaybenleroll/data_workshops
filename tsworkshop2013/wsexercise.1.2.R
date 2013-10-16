
###
### Worksheet Exercise 1.2
###

source('setup_data.R', echo = TRUE);


class(MA.month.ts);
str(MA.month.ts);

start(MA.month.ts); end(MA.month.ts); frequency(MA.month.ts);

plot(MA.month.ts, ylab = "Unemployment data for the state of Maine");


### Using ggplot2 looks better, but you have to work hard for the
### labels on the x-axis so I am leaving this out for now.

#qplot(1:length(Maine.month.ts), as.vector(Maine.month.ts), geom = 'line', ylab = 'Unemployment data for the state of Maine');
