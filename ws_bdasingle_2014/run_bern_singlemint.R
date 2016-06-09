
source("setup_data.R", echo = TRUE);

### Set a seed to ensure that the random processes are repeatable.
set.seed(42);

sample.count <- 1000;

chain.count  <- 3;
adapt.steps  <- 500;
burnin.steps <- 1000;

jags.file <- 'singlemint_bern.jag';



generate.jags.bernouilli.samples <- function(mintdata.dt) {
    data.nTrialTotal <- dim(mintdata.dt)[1];
    data.nCoins      <- length(unique(mintdata.dt$coinid));
    data.coin        <- match(mintdata.dt$coinid, unique(mintdata.dt$coinid));
    data.y           <- mintdata.dt$outcome;

    data.jags <- list(nTrialTotal = data.nTrialTotal,
                      nCoins      = data.nCoins,
                      coin        = data.coin,
                      y           = data.y);

    jagsModel <- jags.model(jags.file, data = data.jags, n.chains = chain.count, n.adapt = adapt.steps);

    update(jagsModel, n.iter = burnin.steps);

    coda.data <- coda.samples(jagsModel, variable.names = c('mu', 'kappa'), n.iter = sample.count);

    return(coda.data);
}


### Setup and run the model for mint 1
coda.bern.mint1 <- generate.jags.bernouilli.samples(use.data.dt[mintid == 1]);
coda.bern.mint2 <- generate.jags.bernouilli.samples(use.data.dt[mintid == 2]);
coda.bern.mint3 <- generate.jags.bernouilli.samples(use.data.dt[mintid == 3]);

coda.bern.lst <- list(mint1 = coda.bern.mint1,
                      mint2 = coda.bern.mint2,
                      mint3 = coda.bern.mint3);
