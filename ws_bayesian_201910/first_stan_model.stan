data {
  real priorA;
  real priorB;

  real K;

  int N;
  int y[N];
}

parameters {
  real<lower=0,upper=1> theta;
  real<lower=0,upper=1> mu;
}

model {
  real a;
  real b;

  a = mu * K;
  b = (1 - mu) * K;

  mu    ~ beta(priorA, priorB);
  theta ~ beta(a, b);

  y ~ bernoulli(theta);
}

generated quantities {
  int y_check;

  y_check = bernoulli_rng(theta);
}
