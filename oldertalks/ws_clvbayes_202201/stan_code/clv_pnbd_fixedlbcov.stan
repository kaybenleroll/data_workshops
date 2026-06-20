functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;           // number of customers

  real<lower=0> lb_cov;

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed
}


parameters {
  real<lower=0> lb_mean;
  real<lower=0> mu_mean;
  real<lower=0> mu_cov;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // expected mean lifetime
}


transformed parameters {
  // shape <- 1 / (cv^2)
  // scale <- mu * cv^2

  real<lower=0> r = 1 / (lb_cov * lb_cov);
  real<lower=0> s = 1 / (mu_cov * mu_cov);

  real<lower=0> alpha = 1 / (lb_mean * lb_cov * lb_cov);
  real<lower=0> beta  = 1 / (mu_mean * mu_cov * mu_cov);
}


model {
  lb_mean ~ lognormal(-2.5, 0.50);
  mu_mean ~ lognormal(-2.5, 0.50);

  mu_cov  ~ lognormal(-0.125, 0.50);

  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s, beta);

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}
