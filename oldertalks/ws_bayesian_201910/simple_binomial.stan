data {
  real prior_sh1;
  real prior_sh2;
  real K;

  int n;
  int k;

  int prior_pd;
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

  mu    ~ beta(prior_sh1, prior_sh2);
  theta ~ beta(a, b);

  if(prior_pd == 1)
    k ~ binomial(n, theta);
}

generated quantities {
  int k_ppd;

  k_ppd = binomial_rng(n, theta);
}
