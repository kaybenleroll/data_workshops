data {
  int<lower=1> n;                   // count of observations

  array[n] int<lower=0> btyd_count; // observed number of transactions
  vector<lower=0>[n]    tnx_weeks;  // observed time-period of transactions
}

parameters {
  vector<lower=0>[n] lambda;
}

model {
  target += poisson_lpmf(btyd_count | lambda .* tnx_weeks);
}

