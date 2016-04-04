data {
  int n_mints;
  int n_coins;

  int trials[n_coins];
  int success[n_coins];
  int mint_id[n_coins];

  real priorShape;
  real priorRate;

  real priorA;
  real priorB;
}

parameters {
  real<lower=0,upper=1> mu[n_mints];
  real<lower=0> K[n_mints];

  real<lower=0,upper=1> theta[n_coins];
}

model {
  real a;
  real b;

  priorA <- 2;
  priorB <- 2;

  a <- mu * K;
  b <- (1 - mu) * K;

  for(i in 1:n_coins) {
    success[i] ~ binomial(trials[i], theta[i]);

    theta[i] ~ beta(a[mint[i]], b[mint[i]])
  }

  for(i in 1:n_mints) {
    a[mint_id] <- mu[mint_id] * K[mint_id];
    b[mint_id] <- (1.0 - mu[mint_id]) * K[mint_id];

    mu[mint_id] ~ beta(priorA, priorB);
    K[mint_id]  ~ gamma(priorShape, priorRate);
  }
}
