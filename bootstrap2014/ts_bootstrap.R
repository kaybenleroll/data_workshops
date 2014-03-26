


lynx.fun <- function(tsb) {
    fit <- ar(tsb, order.max=25)
    return(c(fit$order, mean(tsb)))
}


ts.bootstrap <- tsboot(log(lynx), lynx.fun, R=999, sim="geom", l = 20);
