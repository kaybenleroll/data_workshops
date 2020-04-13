source("lib.R");

p10  <- seq(9.5, 0.5, by = -1);
pd10 <- rep(1, 10);
p0   <- c(seq(4.5, 0.5, by = -1), rep(0, 5));

A <- rbind(p10, pd10, p0);
y <- c(1, 0, 0);


x <- MASS::ginv(A) %*% y;

sqrt(sum(x * x))

x <- corpcor::pseudoinverse(A) %*% y;

sqrt(sum(x * x))


T1 <- pracma::Toeplitz(rep(1, 10), c(1, rep(0, 9)));

pdot <- T1 %*% x;


T2 <- pracma::Toeplitz(rev(p10), c(0.5, rep(0, 9)));

p <- T2 %*% x;
