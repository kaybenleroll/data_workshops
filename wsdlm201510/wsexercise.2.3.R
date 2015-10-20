source("lib.R");


G1 <- matrix(c(1.0, 0.2, 0.1
             , 0.1, 2.0, 0.1
             , 0.3, 0.1, 3.0), ncol = 3, byrow = TRUE);

gamma1 <- 3.0;
alpha1 <- 1.2;
sigma1 <- 0.01;



data1 <- create.power.SINR.data(G1, gamma1, alpha1, sigma1, p0 = c(0.1, 0.1, 0.1), n_iter = 50);
plot.power.SINR.data(data1)


data2 <- create.power.SINR.data(G1, gamma1, alpha1, sigma1, p0 = c(0.1, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data2)


data3 <- create.power.SINR.data(G1, gamma1, alpha1, sigma1, p0 = c(1.0, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data3)
