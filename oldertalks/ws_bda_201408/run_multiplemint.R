
source("setup_data.R", echo = TRUE);

### Set a seed to ensure that the random processes are repeatable.
set.seed(42);

sample.count <- 1000;

chain.count  <- 3;
adapt.steps  <- 500;
burnin.steps <- 1000;

jags.file <- 'multiplemint_beta.jag';


mintdata.dt <- use.data.dt[, list(success = sum(outcome), tosscount = dim(.SD)[1]), by = list(mintid, coinid)]

data.nCoins      <- length(unique(mintdata.dt$coinid));
data.tosscount   <- mintdata.dt$tosscount
data.nMints      <- length(unique(mintdata.dt$mintid));
data.mint        <- mintdata.dt$mintid;
data.y           <- mintdata.dt$success;


data.jags <- list(nCoins    = data.nCoins,
                  tosscount = data.tosscount,
                  nMints    = data.nMints,
                  mint      = data.mint,
                  y         = data.y);


jagsModel <- jags.model(jags.file, data = data.jags, n.chains = chain.count, n.adapt = adapt.steps);

update(jagsModel, n.iter = burnin.steps);

multiplemint.coda.data <- coda.samples(jagsModel, variable.names = c('mu', 'kappa'), n.iter = sample.count);
