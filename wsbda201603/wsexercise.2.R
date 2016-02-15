source("lib.R");


#####
##### Exercise 2.1
#####

theta.seq <- seq(0, 1, by = 0.001)

beta0101 <- dbeta(theta.seq,  1,  1);
beta0202 <- dbeta(theta.seq,  2,  2);
beta1010 <- dbeta(theta.seq, 10, 10);
beta1005 <- dbeta(theta.seq, 10,  5);

plot.dt <- rbind(data.table(distribution = "Beta(1,1)",   theta = theta.seq, prob.dens = beta0101)
                ,data.table(distribution = "Beta(2,2)",   theta = theta.seq, prob.dens = beta0202)
                ,data.table(distribution = "Beta(10,10)", theta = theta.seq, prob.dens = beta1010)
                ,data.table(distribution = "Beta(10,5)",  theta = theta.seq, prob.dens = beta1005)
                 );

qplot(theta, prob.dens, data = plot.dt, geom = 'line', colour = distribution);



#####
##### Exercise 2.2
#####

cointoss10 <- readRDS("cointoss10.rds");

H.count <- sum(cointoss10);
T.count <- length(cointoss10) - H.count;

beta0101.posterior <- dbeta(theta.seq, 1 + H.count, 1 + T.count);

qplot(theta.seq, beta0101, geom = 'line') + geom_line(aes(y = beta0101.posterior), color = 'red')


beta0202.posterior <- dbeta(theta.seq, 2 + H.count, 2 + T.count);

qplot(theta.seq, beta0202,           geom = 'line') + geom_line(aes(y = beta0202.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta0202.posterior), color = 'red')


beta1010.posterior <- dbeta(theta.seq, 10 + H.count, 10 + T.count);

qplot(theta.seq, beta1010,           geom = 'line') + geom_line(aes(y = beta1010.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta1010.posterior), color = 'red')


beta1005.posterior <- dbeta(theta.seq, 10 + H.count, 5 + T.count);

qplot(theta.seq, beta1005,           geom = 'line') + geom_line(aes(y = beta1005.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta1005.posterior), color = 'red')



#####
##### Exercise 2.3
#####

cointoss1000 <- readRDS("cointoss1000.rds");

H.count <- sum(cointoss1000);
T.count <- length(cointoss1000) - H.count;

beta0101.posterior <- dbeta(theta.seq, 1 + H.count, 1 + T.count);

qplot(theta.seq, beta0101, geom = 'line') + geom_line(aes(y = beta0101.posterior), color = 'red')


beta0202.posterior <- dbeta(theta.seq, 2 + H.count, 2 + T.count);

qplot(theta.seq, beta0202,           geom = 'line') + geom_line(aes(y = beta0202.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta0202.posterior), color = 'red')


beta1010.posterior <- dbeta(theta.seq, 10 + H.count, 10 + T.count);

qplot(theta.seq, beta1010,           geom = 'line') + geom_line(aes(y = beta1010.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta1010.posterior), color = 'red')


beta1005.posterior <- dbeta(theta.seq, 10 + H.count, 5 + T.count);

qplot(theta.seq, beta1005,           geom = 'line') + geom_line(aes(y = beta1005.posterior), color = 'red')
qplot(theta.seq, beta0101.posterior, geom = 'line') + geom_line(aes(y = beta1005.posterior), color = 'red')



#####
##### Exercise 2.4
#####

qplot(theta.seq, beta0101.posterior, geom = 'line', xlim = c(0.60, 0.75)) +
    geom_line(aes(y = beta1005.posterior), color = 'red')
