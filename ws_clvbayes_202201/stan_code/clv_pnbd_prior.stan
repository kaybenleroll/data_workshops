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
  lb_mean ~ lognormal(-1.6, 1.00);
  mu_mean ~ lognormal(-2.1, 1.00);

  lb_cov  ~ lognormal(-0.02,   0.20);
  mu_cov  ~ lognormal(-0.0002, 0.02);

  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s, beta);

print("params:", lb_mean, " ", lb_cov, " ", mu_mean, " ", mu_cov);
print("distrib:", r, " ", alpha, " ", s, " ", beta);
print("   ");

  // likelihood
  vector[n] lht;
  vector[n] rht;

  lht = log(lambda) - (lambda + mu) .* T_cal;
  rht = log(mu)     - (lambda + mu) .* t_x;

  target += x .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(lht[i], rht[i]);
  }
}

