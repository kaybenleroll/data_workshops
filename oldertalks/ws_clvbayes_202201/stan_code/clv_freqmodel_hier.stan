data {
  int<lower=1> n;        // count of observations

  int<lower=0> x[n];     // observed number of transactions
  vector<lower=0>[n] t;  // observed time-period of transactions
}


parameters {
  real<lower=0> lb_mean;
  real<lower=0> lb_cov;

  vector<lower=0>[n] lambda;
}


transformed parameters {
  // shape <- 1 / (cv^2)
  // scale <- mu * cv^2

  real<lower=0> r     = 1 / (lb_cov * lb_cov);
  real<lower=0> alpha = 1 / (lb_mean * lb_cov * lb_cov);
}


model {
  lb_mean ~ lognormal(-1.4, 0.50);
  lb_cov  ~ lognormal(-0.02, 0.2);

  lambda ~ gamma(r, alpha);

  target += poisson_lpmf(x | lambda .* t);
}

