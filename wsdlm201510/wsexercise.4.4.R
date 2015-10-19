source("lib.R");


A <- matrix(c(-0.1, 0.1, 0, 0.0, -0.2, 0.2, 0.0, 0.0, -0.0), ncol = 3);

h <- 0.01;

A_update <- (diag(3) + h *A);

n_steps <- 10000;

x <- matrix(0, ncol = n_steps, nrow = 3);

x[, 1] <- c(1, 0, 0);


for(i in 2:n_steps) {
    x[, i] <- A_update %*% x[, i-1];
}

qplot((Var2 - 1) * h, value, data = melt(x), geom = 'line', colour = as.character(Var1));
