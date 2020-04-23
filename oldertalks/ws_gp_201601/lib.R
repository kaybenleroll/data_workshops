require(data.table);
require(ggplot2);
require(kernlab);


### This is probably best implemented in another way.
## calc_covar <- function(X1, X2, l=1) {
##   Sigma <- matrix(rep(0, length(X1) * length(X2)), nrow=length(X1))

##   for (i in 1:nrow(Sigma)) {
##     for (j in 1:ncol(Sigma)) {
##       Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
##     }
##   }

##   return(Sigma)
## }

calc_covar <- function(X1, X2, l=1) {
  Sigma <- outer(X1, X2, function(a, b) exp(-0.5 * (abs(a - b) / l)^2));

  return(Sigma)
}




regression_func  <- function(x) { x[1]^2 - 2 * x[1] + 0.25 * x[2]^2 }

add_additive_noise <- function(x, var) { return(x + rnorm(length(x), 0, var)) }
