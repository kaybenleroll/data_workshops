functions {
  #include util_functions.stan
}

data {
  // hierarchical prior parameters
  real lb_mean_a, lb_mean_b;
  real lb_cov_a,  lb_cov_b;
  real mu_mean_a, mu_mean_b;
  real mu_cov_a,  mu_cov_b;

  // model data
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed
}


parameters {
  real<lower=0> lb_mean;
  real<lower=0> mu_mean;

  real<lower=0> lb_cov;
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
  lb_mean ~ lognormal(lb_mean_a, lb_mean_b);
  lb_cov  ~ lognormal(lb_cov_a,  lb_cov_b);

  mu_mean ~ lognormal(mu_mean_a, mu_mean_b);
  mu_cov  ~ lognormal(mu_cov_a,  mu_cov_b);

  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s, beta);

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}

