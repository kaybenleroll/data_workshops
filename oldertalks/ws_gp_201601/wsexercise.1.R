source("lib.R");


#####
##### Exercise 1.1
#####

N <- 1000
mu <- 0.0003
sigma <- 0.01

data <- rnorm(1000, mu, sigma)



#####
##### Exercise 1.2
#####

summary(data);
qplot(data, geom = 'density');

x_seq <- seq(0, 1, by = 0.01)

qplot(qnorm(x_seq, mean = 0.0003, sd = 0.01)
     ,quantile(data, x_seq)
     ,geom = 'line')



#####
##### Exercise 1.3
#####

N <- 1000
Mu <- c(1.0, 1.0)
Sigma <- matrix(c(1.0, 0.5, 0.5, 1.0), ncol = 2)

data <- MASS::mvrnorm(N, Mu, Sigma)



#####
##### Exercise 1.4
#####

apply(data, 2, mean)
apply(data, 2, sd)
cov(data)

qplot(data[,1], data[,2], geom = 'density2d')



#####
##### Exercise 1.5
#####

N <- 1000000
Mu <- c(1.0, 1.0)
Sigma <- matrix(c(1.0, 0.5, 0.5, 1.0), ncol = 2)

data <- MASS::mvrnorm(N, Mu, Sigma)

cond_data <- data[abs(data[,1] - 0.5) < 0.01,]
dim(cond_data)
mean(cond_data[,2])
sd(cond_data[,2])

cond_data <- data[abs(data[,1] - 0.5) < 0.005,]
dim(cond_data)
mean(cond_data[,2])
sd(cond_data[,2])

qplot(cond_data[,1], cond_data[,2], geom = 'density2d')



#####
##### Exercise 1.6
#####

N <- 1000000
Mu <- c(1.0, 1.0, 1.0)
Sigma <- matrix(c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0), ncol = 3)

data <- MASS::mvrnorm(N, Mu, Sigma)



#####
##### Exercise 1.7
#####

x_seq <- seq(-1, 1, by = 0.01);

sigma <- calc_covar(x_seq, x_seq, 1);

gp_data_010 <- MASS::mvrnorm(50, rep(0, length(x_seq)), sigma);

plot_010_dt <- melt(gp_data_010);
setDT(plot_010_dt);

plot_010_dt[, x := x_seq[Var2]];

qplot(x, value, data = plot_010_dt, geom = 'line', group = Var1, size = I(0.3),
      xlab = expression(x), ylab = expression(f(x)))



#####
##### Exercise 1.8
#####

### Step size of 0.001
x_seq <- seq(-1, 1, by = 0.001);

sigma <- calc_covar(x_seq, x_seq, 1);

gp_data_001 <- MASS::mvrnorm(50, rep(0, length(x_seq)), sigma);

plot_001_dt <- melt(gp_data_001);
setDT(plot_001_dt);

plot_001_dt[, x := x_seq[Var2]];

qplot(x, value, data = plot_001_dt, geom = 'line', group = Var1, size = I(0.3),
      xlab = expression(x), ylab = expression(f(x)))

### Create process for 0.1 step size
x_seq <- seq(-1, 1, by = 0.1);

sigma <- calc_covar(x_seq, x_seq, 1);

gp_data_100 <- MASS::mvrnorm(50, rep(0, length(x_seq)), sigma);

plot_100_dt <- melt(gp_data_100);
setDT(plot_100_dt);

plot_100_dt[, x := x_seq[Var2]];

qplot(x, value, data = plot_100_dt, geom = 'line', group = Var1, size = I(0.3),
      xlab = expression(x), ylab = expression(f(x)))
