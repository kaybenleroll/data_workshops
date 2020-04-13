source("lib.R");

data_dt <- data.table(x=c(-4,-3,-2,-1, 0, 1, 2, 4),
                      y=c(-2, 0, 1, 1, 2, 2,-1, 1))


#####
##### Exercise 2.1
#####

x_seq   <- seq(-5, 5, by = 0.01);
kxx_inv <- solve(calc_covar(data_dt$x, data_dt$x));

Mu    <- calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% data_dt$y;
Sigma <- calc_covar(x_seq, x_seq) -
    calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% calc_covar(data_dt$x, x_seq);



#####
##### Exercise 2.2
#####

gp_data <- MASS::mvrnorm(100, Mu, Sigma);

plot_dt <- melt(gp_data);
setDT(plot_dt);

plot_dt[, x := x_seq[Var2]];

ggplot() +
    geom_line(aes(x, value, group = Var1), data = plot_dt, size = I(0.3), alpha = I(0.2)) +
    geom_point(aes(x, y), data = data_dt, colour = 'red') +
    xlab(expression(x)) +
    ylab(expression(f(x)));



#####
##### Exercise 2.3
#####

plot_dt[x == 2.5, quantile(value, c(0.1, 0.9))]



#####
##### Exercise 2.4
#####

gp_data_1000 <- MASS::mvrnorm(1000, Mu, Sigma);

plot_1000_dt <- melt(gp_data_1000);
setDT(plot_1000_dt);

plot_1000_dt[, x := x_seq[Var2]];

plot_1000_dt[x == 2.5, quantile(value, c(0.1, 0.9))]



#####
##### Exercise 2.5
#####

x_seq   <- seq(-5, 5, by = 0.005);
kxx_inv <- solve(calc_covar(data_dt$x, data_dt$x));

Mu    <- calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% data_dt$y;
Sigma <- calc_covar(x_seq, x_seq) -
    calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% calc_covar(data_dt$x, x_seq);

gp_data_005 <- MASS::mvrnorm(1000, Mu, Sigma);



plot_005_dt <- melt(gp_data_005);
setDT(plot_005_dt);

plot_005_dt[, x := x_seq[Var2]];

plot_005_dt[x == 2.5, quantile(value, c(0.1, 0.9))]
