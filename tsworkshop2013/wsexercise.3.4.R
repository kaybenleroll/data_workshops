###
### Worksheet Exercise 3.1
###

source('setup_data.R', echo = TRUE);

AP.ts.stl <- stl(AP.ts, s.window = 'periodic');

str(AP.ts.stl);

plot(AP.ts.stl);

### To compare the two, I give an example of comparing the two calculated trend
plot.df <- melt(cbind(decomp = AP.ts.decomp$trend,  stl = AP.ts.stl$time.series[, 'trend']));

head(plot.df);

qplot(Var1, value, data = plot.df, colour = Var2, geom = 'line', ylim = c(0, 500));
