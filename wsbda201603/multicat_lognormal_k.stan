data {
  int n_mints;
  int n_coins;

  int trials[n_coins];
  int success[n_coins];
  int mint_id[n_coins];

  real priorMean;
  real priorSD;

  real priorA;
  real priorB;
}


parameters {
  real<lower=0,upper=1> mu[n_mints];
  real<lower=0> K[n_mints];

  real<lower=0,upper=1> theta[n_coins];
}


transformed parameters {
  real a[n_mints];
  real b[n_mints];

  for(j in 1:n_mints) {
    a[j] <- mu[j] * K[j];
    b[j] <- (1.0 - mu[j]) * K[j];
  }
}


model {
  for(i in 1:n_coins) {
    success[i] ~ binomial(trials[i], theta[i]);
    theta[i]   ~ beta(a[mint_id[i]], b[mint_id[i]]);
  }

  for(j in 1:n_mints) {
    mu[j] ~ beta(priorA, priorB);
    K[j]  ~ lognormal(priorMean, priorSD);
  }
}
