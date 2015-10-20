require(data.table);
require(ggplot2);
require(gridExtra);


create.power.SINR.data <- function(G, gamma, alpha, sigma, p0, n_iter = 50) {
    N <- dim(G)[1];

    ### Construct A and b from the input parameters
    mask  <- 1 - diag(N);
    numer <- alpha * gamma * G;
    denom <- matrix(rep(diag(G), N), ncol = N);

    A <- mask * (numer / denom)

    b <- alpha * gamma * sigma / diag(G)

    q_mat <- mask * G;

    ### Initialise the output
    pout    <- matrix(0, ncol = n_iter, nrow = N);
    SINRout <- matrix(0, ncol = n_iter, nrow = N);

    pout[,1]    <- p0;
    q           <- sigma + q_mat %*% p0;
    SINRout[,1] <- (diag(G) * pout[,1]) / q;

    ### Iterate over each time interval and outptu the value
    for(i in 1:(n_iter-1)) {
        pout[,i+1] <- A %*% pout[,i] + b;

        q <- sigma + q_mat %*% pout[,i+1];

        SINRout[,i+1] <- (diag(G) * pout[,i+1]) / q;
    }

    output.lst <- list(A = A, b = b, p = pout, SINR = SINRout);

    return(output.lst);
}


plot.power.SINR.data <- function(data) {
    power.plot <- qplot(Var2 - 1, value, data = melt(data$p), geom = 'line', colour = as.character(Var1), size = I(0.5)) +
        xlab('Time') + ylab('Power') +
        expand_limits(y = 0) +
        theme(legend.position = 'bottom') +
        scale_colour_discrete(name = 'Transmitter');

    sinr.plot <- qplot(Var2 - 1, value, data = melt(data$SINR), geom = 'line', colour = as.character(Var1), size = I(0.5)) +
        xlab('Time') + ylab('SINR') +
        expand_limits(y = 0) +
        theme(legend.position = 'bottom') +
        scale_colour_discrete(name = 'Transmitter');


    grid.arrange(power.plot, sinr.plot, nrow = 2);
}
