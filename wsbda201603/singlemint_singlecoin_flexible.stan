data {
  real priorA;
  real priorB;

  int N;
  int y[N];
}

parameters {
  real<lower=0,upper=1> theta;
  real<lower=0,upper=1> mu;
}

model {
  real K;

  real a;
  real b;

  K <- 5;

  a <- mu * K;
  b <- (1 - mu) * K;

  y ~ bernoulli(theta);

  mu    ~ beta(priorA, priorB);
  theta ~ beta(a, b);
}
