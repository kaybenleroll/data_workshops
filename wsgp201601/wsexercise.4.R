source("lib.R");

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))



#####
##### Exercise 4.1
#####

x_seq <- seq(-5, 5, by = 0.01);

noise_sigma <- 0.1;
