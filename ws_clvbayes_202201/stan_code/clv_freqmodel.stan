data {
  int<lower=1> n;        // count of observations

  int<lower=0> x[n];     // observed number of transactions
  vector<lower=0>[n] t;  // observed time-period of transactions
}

parameters {
  vector<lower=0>[n] lambda;
}

model {
  lambda ~ gamma(1, 3);

  target += poisson_lpmf(x | lambda .* t);
}

