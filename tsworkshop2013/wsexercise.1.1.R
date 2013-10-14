###
### Worksheet Exercise 1.1
###

source('setup_data.R', echo = TRUE);


class(AP);
str(AP);

start(AP); end(AP); frequency(AP);

plot(AP, ylab = "Air Passengers (\'000s)");


### Using ggplot2 looks better, but you have to work hard for the
### labels on the x-axis so I am leaving this out for now.

#qplot(1:length(AP), as.vector(AP), geom = 'line', ylab = 'Air Passengers (\'000s)');
