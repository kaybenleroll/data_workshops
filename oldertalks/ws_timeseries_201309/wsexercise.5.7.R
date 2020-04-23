###
### Worksheet Exercise 5.7
###

source('setup_data.R', echo = TRUE);


AP.ts.add.hw <- HoltWinters(AP.ts, seasonal = "add");
plot(AP.ts.add.hw);


AP.ts.mult.hw <- HoltWinters(AP.ts, seasonal = "mult");
plot(AP.ts.mult.hw);
