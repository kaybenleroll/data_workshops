source("lib.R")


#####
##### Exercise 2.1
#####

theta.seq <- seq(0, 1, by = 0.001)

beta0101 <- dbeta(theta.seq,  1,  1)
beta0202 <- dbeta(theta.seq,  2,  2)
beta1010 <- dbeta(theta.seq, 10, 10)
beta1005 <- dbeta(theta.seq, 10,  5)

plot.dt <- rbind(data.table(distribution = "Beta(1,1)",   theta = theta.seq, prob.dens = beta0101)
                ,data.table(distribution = "Beta(2,2)",   theta = theta.seq, prob.dens = beta0202)
                ,data.table(distribution = "Beta(10,10)", theta = theta.seq, prob.dens = beta1010)
                ,data.table(distribution = "Beta(10,5)",  theta = theta.seq, prob.dens = beta1005)
                 )

qplot(theta, prob.dens, data = plot.dt, geom = 'line', colour = distribution)



#####
##### Exercise 2.2
#####

cointoss10 <- readRDS("cointoss10.rds")

H.10.count <- sum(cointoss10)
T.10.count <- length(cointoss10) - H.10.count

beta0101.10.posterior <- dbeta(theta.seq, 1 + H.10.count, 1 + T.10.count)

qplot(theta.seq, beta0101, geom = 'line') + geom_line(aes(y = beta0101.10.posterior), color = 'red')


beta0202.10.posterior <- dbeta(theta.seq, 2 + H.10.count, 2 + T.10.count)

qplot(theta.seq, beta0202,              geom = 'line') + geom_line(aes(y = beta0202.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta0202.10.posterior), color = 'red')


beta1010.10.posterior <- dbeta(theta.seq, 10 + H.10.count, 10 + T.10.count)

qplot(theta.seq, beta1010,              geom = 'line') + geom_line(aes(y = beta1010.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta1010.10.posterior), color = 'red')


beta1005.posterior <- dbeta(theta.seq, 10 + H.10.count, 5 + T.10.count)

qplot(theta.seq, beta1005,              geom = 'line') + geom_line(aes(y = beta1005.10.posterior), color = 'red')
qplot(theta.seq, beta0101.10.posterior, geom = 'line') + geom_line(aes(y = beta1005.10.posterior), color = 'red')



#####
##### Exercise 2.3
#####

cointoss1000 <- readRDS("cointoss1000.rds")

H.1000.count <- sum(cointoss1000)
T.1000.count <- length(cointoss1000) - H.1000.count

beta0101.1000.posterior <- dbeta(theta.seq, 1 + H.1000.count, 1 + T.1000.count)

qplot(theta.seq, beta0101, geom = 'line') + geom_line(aes(y = beta0101.1000.posterior), color = 'red')


beta0202.1000.posterior <- dbeta(theta.seq, 2 + H.1000.count, 2 + T.1000.count)

qplot(theta.seq, beta0202,                geom = 'line') + geom_line(aes(y = beta0202.1000.posterior), color = 'red')
qplot(theta.seq, beta0101.1000.posterior, geom = 'line') + geom_line(aes(y = beta0202.1000.posterior), color = 'red')


beta1010.1000.posterior <- dbeta(theta.seq, 10 + H.1000.count, 10 + T.1000.count)

qplot(theta.seq, beta1010,                geom = 'line') + geom_line(aes(y = beta1010.1000.posterior), color = 'red')
qplot(theta.seq, beta0101.1000.posterior, geom = 'line') + geom_line(aes(y = beta1010.1000.posterior), color = 'red')


beta1005.1000.posterior <- dbeta(theta.seq, 10 + H.1000.count, 5 + T.1000.count)

qplot(theta.seq, beta1005,                geom = 'line') + geom_line(aes(y = beta1005.1000.posterior), color = 'red')
qplot(theta.seq, beta0101.1000.posterior, geom = 'line') + geom_line(aes(y = beta1005.1000.posterior), color = 'red')



#####
##### Exercise 2.4
#####

N10.0101.plot <- qplot(theta.seq, beta0101, geom = 'line') +
    geom_line(aes(y = beta0101.10.posterior), color = 'red') +
    ylab("Beta(1,1) with 10 datapoints")

N1000.0101.plot <- qplot(theta.seq, beta0101, geom = 'line') +
    geom_line(aes(y = beta0101.1000.posterior), color = 'red') +
    ylab("Beta(1,1) with 1000 datapoints")

N10.1010.plot <- qplot(theta.seq, beta1010, geom = 'line') +
    geom_line(aes(y = beta1010.10.posterior), color = 'red') +
    ylab("Beta(10,10) with 10 datapoints")

N1000.1010.plot <- qplot(theta.seq, beta1010, geom = 'line') +
    geom_line(aes(y = beta1010.1000.posterior), color = 'red') +
    ylab("Beta(10,10) with 1000 datapoints")

grid.arrange(N10.0101.plot, N1000.0101.plot, N10.1010.plot, N1000.1010.plot, nrow = 2)
