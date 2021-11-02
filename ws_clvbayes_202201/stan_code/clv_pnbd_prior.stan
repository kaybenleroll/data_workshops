data {
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
  lb_mean ~ lognormal(-2.3, 0.25);
  mu_mean ~ lognormal(-2.5, 1);

  lb_cov  ~ lognormal(0, 0.2);
  mu_cov  ~ lognormal(0, 1);

  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s, beta);

  // likelihood
  target += x .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(
      log(lambda[i]) - (lambda[i] + mu[i]) .* T_cal[i],
      log(mu[i])     - (lambda[i] + mu[i]) .* t_x[i]
      );
  }
}

