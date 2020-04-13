data {
  int N;
  int y[N];
}

parameters {
  real<lower=0,upper=1> theta;
  real<lower=0,upper=1> mu;
}

model {
  real priorA;
  real priorB;
  real K;

  real a;
  real b;

  K <- 5;
  priorA <- 2;
  priorB <- 2;

  a <- mu * K;
  b <- (1 - mu) * K;

  y ~ bernoulli(theta);

  mu    ~ beta(priorA, priorB);
  theta ~ beta(a, b);
}
