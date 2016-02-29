source("lib.R");


#####
##### Exercise 4.1
#####

cointoss10 <- readRDS("cointoss10.rds")

d0  <- 0.005
dmu <- 0.005

theta.seq <- seq(0 + d0,  1 - d0, by = d0)
mu.seq    <- seq(0 + dmu, 1 - dmu, by = dmu)

mu.prior  <- dbeta(mu.seq, 2, 2)
K         <- 5


hier.posterior.5 <- calculate.hierarchical.posterior(cointoss10, mu.seq, theta.seq, mu.prior, K)

hier.5.plot <- qplot(x = theta.seq[Var1], y = mu.seq[Var2], z = value, data = melt(hier.posterior.5)
                    ,geom = 'contour', xlim = c(0,1), ylim = c(0,1)
                    ,xlab = expression(theta), ylab = expression(mu))



#####
##### Exercise 4.3
#####

d0  <- 0.005
dmu <- 0.005

theta.seq <- seq(0 + d0,  1 - d0, by = d0)
mu.seq    <- seq(0 + dmu, 1 - dmu, by = dmu)

mu.prior  <- dbeta(mu.seq, 2, 2)
K         <- 100


hier.posterior.100 <- calculate.hierarchical.posterior(cointoss10, mu.seq, theta.seq, mu.prior, K)

hier.100.plot <- qplot(x = theta.seq[Var1], y = mu.seq[Var2], z = value, data = melt(hier.posterior.100)
                      ,geom = 'contour',xlim = c(0, 1), ylim = c(0, 1)
                      ,xlab = expression(theta), ylab = expression(mu))
