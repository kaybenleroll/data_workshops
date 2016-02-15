source("lib.R");


#####
##### Exercise 3.1
#####

cointoss10 <- readRDS("cointoss10.rds");

theta.seq <- seq(0, 1, by = 0.001)

beta0101.prior <- dbeta(theta.seq, 1, 1);


posterior.numerical <- calculate.posterior.density(beta0101.prior
                                                  ,cointoss10
                                                  ,theta.seq)
