require(data.table);
require(ggplot2);



create.power.SINR.data <- function(G, gamma, alpha, sigma, p0, n_iter = 50) {
    N <- dim(G)[1];

    mask  <- 1 - diag(N);
    numer <- alpha * gamma * G;
    denom <- matrix(rep(diag(G), N), ncol = N);

    A <- mask * (numer / denom)

    b <- alpha * gamma * sigma / diag(G)

    q_mat <- mask * G;

    pout    <- matrix(0, ncol = n_iter, nrow = N);
    SINRout <- matrix(0, ncol = n_iter, nrow = N);

    pout[,1]    <- p0;
    q           <- sigma + q_mat %*% p0;
    SINRout[,1] <- (diag(G) * pout[,1]) / q;

    for(i in 1:(n_iter-1)) {
        pout[,i+1] <- A %*% pout[,i] + b;

        q <- sigma + q_mat %*% pout[,i+1];

        SINRout[,i+1] <- (diag(G) * pout[,i+1]) / q;
    }

    output.lst <- list(p = pout, SINR = SINRout);

    return(output.lst);
}
