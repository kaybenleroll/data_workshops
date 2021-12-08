data {
  int<lower=1> n;        // count of observations

  int<lower=0> x[n];     // observed number of transactions
  vector<lower=0>[n] t;  // observed time-period of transactions

  real r;
  real alpha;
}

parameters {
  vector<lower=0>[n] lambda;
}

model {
  lambda ~ gamma(r, alpha);

  target += poisson_lpmf(x | lambda .* t);
}

