###
### Worksheet Exercise 4.2
###

source('setup_data.R', echo = TRUE);

AP.ts.acf <- acf(AP.ts, plot = FALSE);


calc.autocor <- function(x, lag) {
    N <- length(x);

    idx1 <- 1:(N-lag);
    idx2 <- (lag+1):N;

    cor(AP.ts[idx1], AP.ts[idx2]);
}


### Adding 1 to the index of the acf as acf[1] is the lag at 0 (i.e. it has value 1)
lag <- 1;
AP.ts.acf$acf[lag+1]
calc.autocor(AP.ts, lag);


lag <- 3;
AP.ts.acf$acf[lag+1]
calc.autocor(AP.ts, lag);


lag <- 12;
AP.ts.acf$acf[lag+1]
calc.autocor(AP.ts, lag);
