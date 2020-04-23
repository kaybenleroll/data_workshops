source("lib.R");


#####
##### Exercise 3.1
#####

cointoss10 <- readRDS("cointoss10.rds")

H.10.count <- sum(cointoss10)
T.10.count <- length(cointoss10) - H.10.count


theta.seq <- seq(0, 1, by = 0.001)

beta0101.prior <- dbeta(theta.seq, 1, 1)


posterior.numerical <- calculate.posterior.density(beta0101.prior
                                                  ,cointoss10
                                                  ,theta.seq)

posterior.analytic <- dbeta(theta.seq, 1 + H.10.count, 1 + T.10.count)


qplot(theta.seq, posterior.numerical, geom = 'line') + geom_line(aes(y = posterior.analytic), color = 'red')



#####
##### Exercise 3.2
#####

cointoss10 <- readRDS("cointoss10.rds")

H.10.count <- sum(cointoss10)
T.10.count <- length(cointoss10) - H.10.count

beta0101 <- dbeta(theta.seq,  1,  1)
beta0202 <- dbeta(theta.seq,  2,  2)
beta1010 <- dbeta(theta.seq, 10, 10)
beta1005 <- dbeta(theta.seq, 10,  5)

beta0101.10.posterior <- calculate.posterior.density(beta0101, cointoss10, theta.seq)

qplot(theta.seq, beta0101, geom = 'line') + geom_line(aes(y = beta0101.10.posterior), color = 'red')


beta0202.10.posterior <- calculate.posterior.density(beta0202, cointoss10, theta.seq)

qplot(theta.seq, beta0202,              geom = 'line') + geom_line(aes(y = beta0202.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta0202.10.posterior), color = 'red')


beta1010.10.posterior <- calculate.posterior.density(beta1010, cointoss10, theta.seq)

qplot(theta.seq, beta1010,              geom = 'line') + geom_line(aes(y = beta1010.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta1010.10.posterior), color = 'red')


beta1005.10.posterior <- calculate.posterior.density(beta1005, cointoss10, theta.seq)

qplot(theta.seq, beta1005,              geom = 'line') + geom_line(aes(y = beta1005.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta1005.10.posterior), color = 'red')


#####
##### Exercise 3.3
#####

biased.prior <- pmax(0, 0.25 + ifelse(theta.seq < 0.25, theta.seq - 0.25, 0.25 - theta.seq))
biased.prior <- biased.prior + rev(biased.prior)
biased.prior <- biased.prior / sum(biased.prior * (theta.seq[2] - theta.seq[1]))

qplot(theta.seq, biased.prior, geom = 'line')

biased.posterior <- calculate.posterior.density(biased.prior, cointoss10, theta.seq)
qplot(theta.seq, biased.prior, geom = 'line') + geom_line(aes(y = biased.posterior), color = 'red')


biased.prior.2     <- 0.5 * (dbeta(theta.seq, 25, 75) + dbeta(theta.seq, 75, 25))
qplot(theta.seq, biased.prior.2, geom = 'line')

biased.posterior.2 <- calculate.posterior.density(biased.prior.2, cointoss10, theta.seq)
qplot(theta.seq, biased.prior.2, geom = 'line') + geom_line(aes(y = biased.posterior.2), color = 'red')


biased.prior.3     <- 0.5 * (dbeta(theta.seq, 250, 750) + dbeta(theta.seq, 750, 250))
qplot(theta.seq, biased.prior.3, geom = 'line')

biased.posterior.3 <- calculate.posterior.density(biased.prior.3, cointoss10, theta.seq)
qplot(theta.seq, biased.prior.3, geom = 'line') + geom_line(aes(y = biased.posterior.3), color = 'red')



#####
##### Exercise 3.4
#####

d0 <- theta.seq[2] - theta.seq[1]

part.1 <- pmax(0, 0.25 + ifelse(theta.seq <= 0.25, theta.seq - 0.25, 0.25 - theta.seq))
part.2 <- rev(part.1)
part.3 <- pmax(0, 0.50 + ifelse(theta.seq <= 0.50, theta.seq - 0.50, 0.50 - theta.seq))

threeposs.prior <- part.1 + part.2 + part.3
threeposs.prior <- threeposs.prior / sum(threeposs.prior * d0)

qplot(theta.seq, threeposs.prior, geom = 'line')

threeposs.posterior <- calculate.posterior.density(threeposs.prior, cointoss10, theta.seq)
qplot(theta.seq, threeposs.prior, geom = 'line') + geom_line(aes(y = threeposs.posterior), color = 'red')
