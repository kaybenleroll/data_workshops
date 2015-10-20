source("lib.R");


calculate.xmu <- function(mu, A, y) {
    AtA <- t(A) %*% A;

    N <- dim(AtA)[1];

    invmat <- solve(AtA + mu * diag(rep(1, N)));

    xmu <- invmat %*% (t(A) %*% y)

    return(xmu);
}
