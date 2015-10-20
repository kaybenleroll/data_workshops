source("lib.R");


G2 <- matrix(c(1.0, 0.2, 0.1
             , 0.1, 2.0, 0.1
             , 0.3, 0.1, 3.0), ncol = 3, byrow = TRUE);

gamma2 <- 5.0;
alpha2 <- 1.2;
sigma2 <- 0.01;



data4 <- create.power.SINR.data(G2, gamma2, alpha2, sigma2, p0 = c(0.1, 0.1, 0.1), n_iter = 50);
plot.power.SINR.data(data4)


data5 <- create.power.SINR.data(G2, gamma2, alpha2, sigma2, p0 = c(0.1, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data5)


data6 <- create.power.SINR.data(G2, gamma2, alpha2, sigma2, p0 = c(1.0, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data6)
