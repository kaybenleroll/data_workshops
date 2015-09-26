
source("setup_data.R", echo = TRUE);


### Using data.table's functionality, it is easy to calculate MLE estimations of the theta for each coin
### For a binary (0, 1) outcome, this is just the mean of the outcome
use.data.dt[, list(theta = mean(outcome)), by = list(mintid, coinid)];


### We can then get an estimate of the mint mu by averaging over all the thetas for that mint
use.data.dt[, list(theta = mean(outcome)), by = list(mintid, coinid)][, list(mu = mean(theta)), by = list(mintid)];


### Density estimation is very easy to visualise in ggplot2
qplot(theta, data = use.data.dt[, list(theta = mean(outcome)), by = list(mintid, coinid)], geom = 'density', group = mintid, colour = as.factor(mintid));
