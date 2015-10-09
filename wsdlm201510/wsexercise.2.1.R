

G <- matrix(c(1.0, 0.2, 0.1,
              0.1, 2.0, 0.1,
              0.3, 0.1, 3.0), ncol = 3, byrow = TRUE);


gamma <- 3.0;
alpha <- 1.2;
sigma <- 0.01;



N <- dim(G)[1];

mask  <- 1 - diag(N);
numer <- alpha * gamma * G;
denom <- matrix(rep(diag(G), N), ncol = N);

A <- mask * (numer / denom)

b <- alpha * gamma * sigma / diag(G)

q_mat <- mask * G;


n_iter <- 25;

pout    <- matrix(0, ncol = n_iter, nrow = N);
SINRout <- matrix(0, ncol = n_iter, nrow = N);


p0 <- rep(0.1, N);

pout[,1]    <- p0;
q           <- sigma + q_mat %*% p0;
SINRout[,1] <- (diag(G) * pout[,1]) / q;

for(i in 1:(n_iter-1)) {
    pout[,i+1] <- A %*% pout[,i] + b;

    q <- sigma + q_mat %*% pout[,i+1];

    SINRout[,i+1] <- (diag(G) * pout[,i+1]) / q;
}



qplot(Var2, value, data = melt(pout), geom = 'line', colour = as.character(Var1), size = I(0.1)) +
    xlab('Time') + ylab('Power') +
    expand_limits(y = 0) +
    theme(legend.position = 'bottom') +
    scale_colour_discrete(name = 'Transmitter');

qplot(Var2, value, data = melt(SINRout), geom = 'line', colour = as.character(Var1), size = I(0.1)) +
    xlab('Time') + ylab('SINR') +
    expand_limits(y = 0) +
    theme(legend.position = 'bottom') +
    scale_colour_discrete(name = 'Transmitter');
