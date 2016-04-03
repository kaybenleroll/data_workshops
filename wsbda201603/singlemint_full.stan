data {
  int n_coins;

  int trials[n_coins];
  int success[n_coins];

  real priorShape;
  real priorRate;
}

parameters {
  real<lower=0,upper=1> theta[n_coins];
  real<lower=0,upper=1> mu;

  real<lower=0> K;
}

model {
  real priorA;
  real priorB;

  real a;
  real b;

  priorA <- 2;
  priorB <- 2;

  a <- mu * K;
  b <- (1 - mu) * K;

  for(i in 1:n_coins) {
    success[i] ~ binomial(trials[i], theta[i]);
  }

  mu    ~ beta(priorA, priorB);
  theta ~ beta(a, b);

  K ~ gamma(priorShape, priorRate);
}
