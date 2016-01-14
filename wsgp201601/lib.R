require(data.table);
require(ggplot2);



### This is probably best implemented in another way.
calcSigma <- function(X1, X2, l=1) {
  Sigma <- matrix(rep(0, length(X1) * length(X2)), nrow=length(X1))

  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }

  return(Sigma)
}
