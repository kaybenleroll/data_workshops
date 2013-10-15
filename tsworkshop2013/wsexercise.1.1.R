###
### Worksheet Exercise 1.1
###

source('setup_data.R', echo = TRUE);


class(AP.ts);
str(AP.ts);

start(AP.ts); end(AP.ts); frequency(AP.ts);

plot(AP.ts, ylab = "Air Passengers (\'000s)");


### Using ggplot2 looks better, but you have to work hard for the
### labels on the x-axis so I am leaving this out for now.

#qplot(1:length(AP.ts), as.vector(AP.ts), geom = 'line', ylab = 'Air Passengers (\'000s)');
