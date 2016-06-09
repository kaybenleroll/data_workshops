source("lib.R");

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))


#####
##### Exercise 3.1
#####

x_seq <- seq(-5, 5, by = 0.01);

noise_sigma <- 0.1;

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))

kxx_inv <- solve(calc_covar(data_dt$x, data_dt$x) + noise_sigma^2 * diag(1, nrow(data_dt)));

Mu    <- calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% data_dt$y;
Sigma <- calc_covar(x_seq, x_seq) -
    calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% calc_covar(data_dt$x, x_seq);



#####
##### Exercise 3.2
#####

gp_noisydata_01 <- MASS::mvrnorm(50, Mu, Sigma);

plot_noisydata_dt <- melt(gp_noisydata_01);
setDT(plot_noisydata_dt);

plot_noisydata_dt[, x := x_seq[Var2]];

ggplot() +
    geom_line(aes(x, value, group = Var1), data = plot_noisydata_dt, size = I(0.3), alpha = I(0.2)) +
    geom_point(aes(x, y), data = data_dt, colour = 'red') +
    xlab(expression(x)) +
    ylab(expression(f(x)));


## data_dt[, ymin := y - 3 * noise_sigma];
## data_dt[, ymax := y + 3 * noise_sigma];

## ggplot() +
##     geom_line(aes(x, value, group = Var1), data = plot_dt, size = I(0.3), alpha = I(0.2)) +
##     geom_point(aes(x, y), data = data_dt, colour = 'red') +
##     geom_errorbar(aes(x = x, ymin = ymin, ymax = ymax), data = data_dt, colour = 'red', width = 0.1) +
##     xlab(expression(x)) +
##     ylab(expression(f(x)));


#####
##### Exercise 3.3
#####

gp_noisydata_01 <- MASS::mvrnorm(1000, Mu, Sigma);

plot_noisydata_dt <- melt(gp_noisydata_01);
setDT(plot_noisydata_dt)

plot_noisydata_dt[, x := x_seq[Var2]];

plot_noisydata_dt[x == 2.5, quantile(value, c(0.1, 0.9))]



#####
##### Exercise 3.4
#####

x_seq <- seq(-5, 5, by = 0.005);

noise_sigma <- 0.1;

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))

kxx_inv <- solve(calc_covar(data_dt$x, data_dt$x) + noise_sigma^2 * diag(1, nrow(data_dt)));

Mu    <- calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% data_dt$y;
Sigma <- calc_covar(x_seq, x_seq) -
    calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% calc_covar(data_dt$x, x_seq);

gp_noisydata_005 <- MASS::mvrnorm(1000, Mu, Sigma);

plot_noisydata_005_dt <- melt(gp_noisydata_005);
setDT(plot_noisydata_005_dt);

plot_noisydata_005_dt[, x := x_seq[Var2]];

plot_noisydata_005_dt[x == 2.5, quantile(value, c(0.1, 0.9))]
