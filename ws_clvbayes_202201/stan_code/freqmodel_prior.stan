data {
  int<lower=1> n;               // count of observations

  int<lower=0> btyd_count[n];   // observed number of transactions
  vector<lower=0>[n] tnx_weeks; // observed time-period of transactions

  real r;
  real alpha;
}

parameters {
  vector<lower=0>[n] lambda;
}

model {
  lambda ~ gamma(r, alpha);

  target += poisson_lpmf(btyd_count | lambda .* tnx_weeks);
}
