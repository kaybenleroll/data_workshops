source("lib.R");

k1 <- 1;
k2 <- 1;

A <- matrix(c(-k1, k1, 0, 0, -k2, k2, 0, 0, 0), ncol = 3);

h <- 0.01;

A_update <- (diag(3) + h *A);

n_steps <- 1000;

x <- matrix(0, ncol = n_steps, nrow = 3);

x[, 1] <- c(1, 0, 0);


for(i in 2:n_steps) {
    x[, i] <- A_update %*% x[, i-1];
}

qplot((Var2 - 1) * h, value, data = melt(x), geom = 'line', colour = as.character(Var1));
