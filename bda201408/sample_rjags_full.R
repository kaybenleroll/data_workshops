library(rjags);


twocoin.data <- readRDS('singlemint_fivecoin.rds');

use.data <- twocoin.data;
sample.count <- 10000;

chain.count  <- 5;
adapt.steps  <- 500;
burnin.steps <- 1000;

jags.file <- 'singlemint_full.jag';


### First we set up the model and check the priors
jagsPriorModel <- jags.model(jags.file, data = list(nTrialTotal = dim(use.data)[2],
                                                    nCoins      = length(unique(use.data['coin', ])),
                                                    coin        = use.data['coin', ]),
                             n.chains = chain.count, n.adapt = adapt.steps);

update(jagsPriorModel, n.iter = burnin.steps);

coda.prior.sample.data <- coda.samples(jagsPriorModel, variable.names = c('mu', 'theta', 'kappa'), n.iter = sample.count);
mcmc.prior.samples     <- as.matrix(coda.prior.sample.data);

### Prior data can be plotted directly
plot(coda.prior.sample.data)


jagsPosteriorModel <- jags.model(jags.file, data = list(nTrialTotal = dim(use.data)[2],
                                                        nCoins      = length(unique(use.data['coin', ])),
                                                        coin        = use.data['coin', ],
                                                        y           = use.data['cointoss', ]),
                                 n.chains = chain.count, n.adapt = adapt.steps);

update(jagsPosteriorModel, n.iter = burnin.steps);

coda.posterior.sample.data <- coda.samples(jagsPosteriorModel, variable.names = c('mu', 'theta', 'kappa'), n.iter = sample.count);
mcmc.posterior.samples     <- as.matrix(coda.posterior.sample.data);
