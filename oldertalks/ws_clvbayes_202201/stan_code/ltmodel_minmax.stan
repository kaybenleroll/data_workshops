data {
  int<lower=1> n;                  // count of observations

  vector<lower=0>[n] min_lifetime; // minimum observed lifetime for customer
  vector<lower=0>[n] max_lifetime; // maximum observed lifetime for customer
  vector<lower=0>[n] obs_time;     // observation time since customer 'birth'

  real s;
  real beta;
}

parameters {
  vector<lower=0>[n] mu;
}

model {
  mu ~ gamma(s, beta);

  target += exponential_lccdf(min_lifetime | mu);
  target += exponential_lcdf (max_lifetime | mu);
}

generated quantities {
#include lifetime_genquan.stan
}
