data {
  int<lower=1> n;       // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed
}


parameters {
  real<lower=0> mu_mean;
  real<lower=0> mu_rate;

  real<lower=0> lambda_mean;
  real<lower=0> lambda_rate;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // expected mean lifetime
}


transformed parameters {
  real<lower=0> mu_shape     = mu_mean     .* mu_rate;
  real<lower=0> lambda_shape = lambda_mean .* lambda_rate;
}


model {
  mu_mean     ~ lognormal(-2.5, 1);
  lambda_mean ~ lognormal(-2.5, 1);

  mu_rate     ~ gamma(10,  1);
  lambda_rate ~ gamma(10,  1);

  mu     ~ gamma(mu_shape,     mu_rate);
  lambda ~ gamma(lambda_shape, lambda_rate);

  // likelihood
  target += x .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(
      log(lambda[i]) - (lambda[i] + mu[i]) .* T_cal[i],
      log(mu[i]) - (lambda[i] + mu[i]) .* t_x[i]
      );
  }
}

