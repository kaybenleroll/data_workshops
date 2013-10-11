### We will be using ggplot2 for the graphing and plyr for some of the data management
require(ggplot2);
require(plyr);

### Source in the helper functions code
source('helper_functions.R');


#########################################################################################################
#########################################################################################################
###
### Exercise 1.1
###
#########################################################################################################
#########################################################################################################


set.seed(42);

disease.data <- generate.disease.test.data(n = 1000000, prior.prob = 0.001, hit.rate = 0.99, false.alarm = 0.05);
calculate.disease.test.probabilities(disease.data);


#########################################################################################################
#########################################################################################################
###
### Exercise 1.2
###
#########################################################################################################
#########################################################################################################


### We use the plyr routine llply here to calculate the conditional probability from the parameters for each value
### of the input probability. The output is a list, so we use unlist() to convert the output to a vector.
### See '?llply' for more information of the llply() routine.


prior.prob.vector <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 0.9);

calc.prob.from.prior <- function(prior.prob) {
    disease.data <- generate.disease.test.data(n = 1000000, prior.prob = prior.prob, hit.rate = 0.99, false.alarm = 0.05);
    cond.prob    <- calculate.disease.test.probabilities(disease.data);

    return(cond.prob);
}

cond.prob.vector <- unlist(llply(prior.prob.vector, calc.prob.from.prior));

### This code creates a line plot of the prior probability vs the conditional probability. The parameter ylim controls
### the limits of the y-axis, giving us a sense of scale
qplot(prior.prob.vector, cond.prob.vector, geom = 'line', ylim = c(0, 1));


### Now we do something similar forthe false.alarm parameter
### I have kept the parameters the same, but it my be instructive to examine the lower end of the scale more closely

fa.prob.vector <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 0.9);

calc.prob.from.fa <- function(fa.prob) {
    disease.data <- generate.disease.test.data(n = 1000000, prior.prob = 0.001, hit.rate = 0.99, false.alarm = fa.prob);
    cond.prob    <- calculate.disease.test.probabilities(disease.data);

    return(cond.prob);
}

cond.prob.vector <- unlist(llply(fa.prob.vector, calc.prob.from.fa));

qplot(prior.prob.vector, cond.prob.vector, geom = 'line', ylim = c(0, 1));


#########################################################################################################
#########################################################################################################
###
### Exercise 1.3
###
#########################################################################################################
#########################################################################################################

### First we generate the data with the default parameters

disease.twotest.data <- generate.disease.twotest.data(n = 1000000, prior.prob = 0.001, hit.rate.1 = 0.99, false.alarm.1 = 0.05, hit.rate.2 = 0.99, false.alarm.2 = 0.05);

disease.twotest.data[, 1:10]   # Look at the first 10 columns of the data to get an idea of the layout

### To speed things up, I will create a quick function to calculate the probability
calc.twotest.prob <- function(data) {
    infected <- data['infected', ]; test.1 <- data['test.1', ]; test.2 <- data['test.2', ];

    return(length(data[infected == 1 & test.1 == 1 & test.2 == 1]) / length(data[test.1 == 1 & test.2 == 1]));
}

calc.twotest.prob(disease.twotest.data);


### Charting the dependencies of this 'twotest' data vs the input parameters is useful here too.
### This introduction to creating contour plots should be useful here. To start with, we are going to show how
### to visualise the effect of multiple variables on a probability density.
### Note how in this code I have 'chained' all the function calls together, for brevity.

twotest.df <- ddply(expand.grid(fa1 = fa.prob.vector, fa2 = fa.prob.vector),
                    .(fa1, fa2),
                    function(df) calc.twotest.prob(generate.disease.twotest.data(false.alarm.1 = df$fa1, false.alarm.2 = df$fa2)));

qplot(fa1, fa2, z = V1, data = twotest.df, geom = 'contour', colour = ..level..) + scale_x_log10() + scale_y_log10();

### To understand this contour plot, you can see that the surface value (representing the conditional probability
### of the person being infected given both tests showing a positive result), forms a steadily declining level
### from very close to 1 when both false alarm rates are small, and gets lower and lower and the false alarm
### rate increases.

### This code tries to show the 3D surface plot for the same data using persp()

twotest.matrix <- matrix(twotest.df$V1, ncol = length(fa.prob.vector));

persp(x = fa.prob.vector, y = fa.prob.vector, z = twotest.matrix, phi = 30, theta = 45, xlim = c(0, 1), ylim = c(0, 1));


#########################################################################################################
#########################################################################################################
###
### Exercise 2.1
###
#########################################################################################################
#########################################################################################################

theta.grid <- seq(0, 1, by = 0.001);

qplot(theta.grid, dbeta(theta.grid, 1, 1), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  5,  5)), colour = 'red') +
    geom_line(aes(y = dbeta(theta.grid, 10, 10)), colour = 'blue') +
    geom_line(aes(y = dbeta(theta.grid, 10, 20)), colour = 'green') +
    geom_line(aes(y = dbeta(theta.grid, 50, 10)), colour = 'purple');



#########################################################################################################
#########################################################################################################
###
### Exercise 2.2
###
#########################################################################################################
#########################################################################################################

### Note: This section shows some plots generated by using a conjugate prior for the Bernouilli likelihood,
### the Beta(a, b) distribution.
###
### For a given data set of coin tosses, the posterior distribution is another Beta distribution with the
### a and b parameters modified by adding the count of heads and tails respectively to a and b.
###
### For more information, the wikipedia entry has a good discussion and explanation of this:
###
### http://en.wikipedia.org/wiki/Conjugate_prior

cointoss10 <- readRDS('cointoss10.rds');

n.heads.10 <- length(cointoss10[cointoss10 == 1]);
n.tails.10 <- length(cointoss10[cointoss10 == 0]);

### Try the data with the beta(1, 1) prior
qplot(theta.grid, dbeta(theta.grid, 1, 1), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  1 + n.heads.10, 1 + n.tails.10)), colour = 'red');


### Try the data with a beta(10, 10) prior
qplot(theta.grid, dbeta(theta.grid, 10, 10), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  10 + n.heads.10, 10 + n.tails.10)), colour = 'red');


#########################################################################################################
#########################################################################################################
###
### Exercise 2.3
###
#########################################################################################################
#########################################################################################################

### This code will be very similar, but will be using the cointoss1000 datafile instead. You may want to
### use different variable names as well, but that is optional.

cointoss1000 <- readRDS('cointoss1000.rds');

n.heads.1000 <- length(cointoss1000[cointoss1000 == 1]);
n.tails.1000 <- length(cointoss1000[cointoss1000 == 0]);

### Try the data with the beta(1, 1) prior, plotting the posterior in red
qplot(theta.grid, dbeta(theta.grid, 1, 1), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  1 + n.heads.1000, 1 + n.tails.1000)), colour = 'red');


### Try the data with a beta(10, 10) prior
qplot(theta.grid, dbeta(theta.grid, 10, 10), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  10 + n.heads.1000, 10 + n.tails.1000)), colour = 'red');


#########################################################################################################
#########################################################################################################
###
### Exercise 2.4
###
#########################################################################################################
#########################################################################################################

### We plot the Beta(1, 1) prior in black, then the 10 toss posterior in red and the 1000 toss in blue.
qplot(theta.grid, dbeta(theta.grid, 1, 1), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,  1 + n.heads.10,   1 + n.tails.10)),   colour = 'red') +
    geom_line(aes(y = dbeta(theta.grid,  1 + n.heads.1000, 1 + n.tails.1000)), colour = 'blue');

qplot(theta.grid, dbeta(theta.grid, 10, 10), geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid,   1 + n.heads.10,    1 + n.tails.10)),   colour = 'red') +
    geom_line(aes(y = dbeta(theta.grid,  10 + n.heads.1000, 10 + n.tails.1000)), colour = 'blue');



#########################################################################################################
#########################################################################################################
###
### Exercise 3.1
###
#########################################################################################################
#########################################################################################################

theta.grid <- seq(0, 1, by = 0.001);
cointoss10 <- readRDS('cointoss10.rds');

posterior.10 <- calculate.posterior.density(dbeta(theta.grid, 1, 1), cointoss10, theta.grid);

qplot(theta.grid, posterior.10, geom = 'line') +
    geom_line(aes(y = dbeta(theta.grid, 1 + n.heads.10, 1 + n.tails.10)), colour = 'blue');


#########################################################################################################
#########################################################################################################
###
### Exercise 3.2
###
#########################################################################################################
#########################################################################################################

prior.weak    <- dbeta(theta.grid,  1,  1);
prior.strong  <- dbeta(theta.grid, 20, 20);

posterior.weak.10   <- calculate.posterior.density(prior.weak,   cointoss10, theta.grid);
posterior.strong.10 <- calculate.posterior.density(prior.strong, cointoss10, theta.grid);

qplot(theta.grid, prior.weak.10,   geom = 'line') + geom_line(aes(y = posterior.weak.10),   colour = 'blue');
qplot(theta.grid, prior.strong.10, geom = 'line') + geom_line(aes(y = posterior.strong.10), colour = 'blue');


posterior.weak.1000   <- calculate.posterior.density(prior.weak,   cointoss1000, theta.grid);
posterior.strong.1000 <- calculate.posterior.density(prior.strong, cointoss1000, theta.grid);

qplot(theta.grid, prior.weak,   geom = 'line') + geom_line(aes(y = posterior.weak.1000),   colour = 'blue');
qplot(theta.grid, prior.strong, geom = 'line') + geom_line(aes(y = posterior.strong.1000), colour = 'blue');



#########################################################################################################
#########################################################################################################
###
### Exercise 3.3
###
#########################################################################################################
#########################################################################################################

### We can create the prior as the combination of two beta priors.

bimodal.prior.10     <- (dbeta(theta.grid, 30, 10) + dbeta(theta.grid, 10, 30)) / 2;
bimodal.posterior.10 <- calculate.posterior.density(bimodal.prior.10, cointoss10, theta.grid);

qplot(theta.grid, bimodal.prior.10, geom = 'line') + geom_line(aes(y = bimodal.posterior.10),   colour = 'blue');


bimodal.prior.1000     <- (dbeta(theta.grid, 30, 10) + dbeta(theta.grid, 10, 30)) / 2;
bimodal.posterior.1000 <- calculate.posterior.density(bimodal.prior.1000, cointoss1000, theta.grid);

qplot(theta.grid, bimodal.prior.1000, geom = 'line') + geom_line(aes(y = bimodal.posterior.1000),   colour = 'blue');


#########################################################################################################
#########################################################################################################
###
### Exercise 3.4
###
#########################################################################################################
#########################################################################################################

trimodal.prior.10     <- (dbeta(theta.grid, 30, 10) + dbeta(theta.grid, 10, 30) + dbeta(theta.grid, 20, 20)) / 3;
trimodal.posterior.10 <- calculate.posterior.density(trimodal.prior.10, cointoss10, theta.grid);

qplot(theta.grid, trimodal.prior.10, geom = 'line') + geom_line(aes(y = trimodal.posterior.10),   colour = 'blue');


trimodal.prior.1000     <- (dbeta(theta.grid, 30, 10) + dbeta(theta.grid, 10, 30) + dbeta(theta.grid, 20, 20)) / 3;
trimodal.posterior.1000 <- calculate.posterior.density(trimodal.prior.1000, cointoss1000, theta.grid);

qplot(theta.grid, trimodal.prior.1000, geom = 'line') + geom_line(aes(y = trimodal.posterior.1000),   colour = 'blue');


#########################################################################################################
#########################################################################################################
###
### Exercise 4.1
###
#########################################################################################################
#########################################################################################################

test.distrib <- c(1, 1, 1, 1);
test.distrib <- test.distrib / sum(test.distrib);

metropolis.sample.1000 <- do.metropolis.island.sampling(test.distrib, sample.count = 1000);


#########################################################################################################
#########################################################################################################
###
### Exercise 4.2
###
#########################################################################################################
#########################################################################################################

distrib.ideal <- unlist(llply(1:length(test.distrib), function(idx) rep(idx, test.distrib[idx] * 1000)));

### The easiest way to compare the two distributions is to create a data.frame that ggplot2 can work with

plot.df <- rbind(data.frame(label = 'ideal',  island = distrib.ideal),
                 data.frame(label = 'sample', island = metropolis.sample.1000));

qplot(island, data = plot.df, fill = label, position = 'dodge', geom = 'bar')


#########################################################################################################
#########################################################################################################
###
### Exercise 4.3
###
#########################################################################################################
#########################################################################################################

test.distrib <- 1:7 / sum(1:7);

metropolis.sample.1000 <- do.metropolis.island.sampling(test.distrib, sample.count = 1000);

distrib.ideal <- unlist(llply(1:length(test.distrib), function(idx) rep(idx, test.distrib[idx] * 1000)));

plot.df <- rbind(data.frame(label = 'ideal',  island = distrib.ideal),
                 data.frame(label = 'sample', island = metropolis.sample.1000));

qplot(island, data = plot.df, fill = label, position = 'dodge', geom = 'bar');
