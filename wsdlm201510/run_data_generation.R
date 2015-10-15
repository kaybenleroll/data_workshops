
alpha <- 0.0001

recv.pos <- list(c( 0, 40)
               , c(10, 10)
               , c(20, 50)
               , c(40, 70)
               , c(50, 10)
               , c(50, 30)
               , c(60, 40)
               , c(70, 30)
               , c(90, 20)
                ,c(90, 70));

trans.pos <- list(c(30, 20)
                , c(60, 70)
                , c(80, 50));

t.seq <- seq(0, 10, by = 0.01);

sig1 <- list(1.3 * sin(2 * pi * t.seq)
           , 0.6 * sin(2 * pi * t.seq + 1.5)
           , 2.1 * sin(2 * pi * t.seq - 0.8)
             );

sig2 <- list(rep(3, length(t.seq))
           , rep(6, length(t.seq))
           , rep(5, length(t.seq))
             );

calculate.receiver.signal <- function(idx, sig) {
    dist.sq <- sapply(trans.pos, function(iterpos) sum((iterpos - recv.pos[[idx]])^2))

    sig.matrix <- do.call('cbind', sig);

    recv.sig <- sig.matrix %*% (1 / (alpha * dist.sq))

    return(recv.sig);
}


received.signal.1 <- sapply(1:length(recv.pos), function(idx) calculate.receiver.signal(idx, sig1));
received.signal.2 <- sapply(1:length(recv.pos), function(idx) calculate.receiver.signal(idx, sig2));
