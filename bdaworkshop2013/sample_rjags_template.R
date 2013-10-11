### Set a seed to ensure that the random processes are repeatable.
set.seed(42);

use.data <- cointoss10;
sample.count <- 10000;

chain.count  <- 5;
adapt.steps  <- 500;
burnin.steps <- 1000;

jags.file <- 'singlemint_singlecoin.jag';


### First we set up the model and check the priors
jagsPriorModel <- jags.model(jags.file, data = list(nFlips = length(use.data)), n.chains = chain.count, n.adapt = adapt.steps);

update(jagsPriorModel, n.iter = burnin.steps);

coda.sample.data <- coda.samples(jagsPriorModel, variable.names = c('theta'), n.iter = sample.count);
mcmc.prior.samples <- as.matrix(coda.sample.data);


jagsPosteriorModel <- jags.model(jags.file, data = list(nFlips = length(use.data), y = use.data), n.chains = chain.count, n.adapt = adapt.steps);

update(jagsPosteriorModel, n.iter = burnin.steps);

coda.sample.data = coda.samples(jagsPosteriorModel, variable.names = c('theta'), n.iter = sample.count);
mcmc.posterior.samples <- as.matrix(coda.sample.data);
