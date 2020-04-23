source("lib.R");

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))



#####
##### Exercise 4.1
#####

data_gausspr <- gausspr(data_dt$x, data_dt$y, variance.model = TRUE, scaled = FALSE, var = 0.001)

prediction.mid <- predict(data_gausspr, newdata = 2.5);
prediction.var <- predict(data_gausspr, newdata = 2.5, type = 'sdeviation')

c(prediction.mid - 1.28 * prediction.var, prediction.mid + 1.28 * prediction.var)



#####
##### Exercise 4.2
#####

data_gausspr <- gausspr(data_dt$x, data_dt$y, variance.model = TRUE, scaled = FALSE, var = 0.1)

prediction.mid <- predict(data_gausspr, newdata = 2.5);
prediction.var <- predict(data_gausspr, newdata = 2.5, type = 'sdeviation')

c(prediction.mid - 1.28 * prediction.var, prediction.mid + 1.28 * prediction.var)



#####
##### Exercise 4.3
#####

points <- cbind(runif(20, -5, 5), runif(20, -5, 5))
values <- apply(points, 1, regression_func)

observed <- add_additive_noise(values, 0.1);



#####
##### Exercise 4.4
#####

nd_gausspr <- gausspr(points, observed, variance.model = TRUE, scaled = FALSE, var = 0.01);

newpoint <- c(-2, -2);
newactual <- regression_func(newpoint);


predict(nd_gausspr, newdata = matrix(newpoint, ncol = 2))
