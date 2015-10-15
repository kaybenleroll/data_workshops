source("lib.R");


G <- matrix(c(1.0, 0.2, 0.1,
              0.1, 2.0, 0.1,
              0.3, 0.1, 3.0), ncol = 3, byrow = TRUE);

gamma <- 3.0;
alpha <- 1.2;
sigma <- 0.01;



data1 <- create.power.SINR.data(G, gamma, alpha, sigma, p0 = c(0.1, 0.1, 0.1), n_iter = 50);
plot.power.SINR.data(data1)


data2 <- create.power.SINR.data(G, gamma, alpha, sigma, p0 = c(0.1, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data2)


data3 <- create.power.SINR.data(G, gamma, alpha, sigma, p0 = c(1, 0.3, 0.2), n_iter = 50);
plot.power.SINR.data(data3)
