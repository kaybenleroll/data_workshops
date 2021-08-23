data {
  int<lower=1> n;       // number of customers

  vector<lower=0>[n] t; // time to most recent purchase
  vector<lower=0>[n] T; // total observation time
  vector<lower=0>[n] k; // number of purchases observed
}


parameters {
  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] etau;   // expected mean lifetime

  real<lower=0> lambda_mean;
  real<lower=0> lambda_rate;

  real<lower=0> etau_mean;
  real<lower=0> etau_rate;
}


transformed parameters {
  vector<lower=0>[n] mu = 1.0 ./ etau;

  real<lower=0> lambda_shape = lambda_mean * lambda_rate;
  real<lower=0> etau_shape   = etau_mean   * etau_rate;

}


model {
  // setting priors
  lambda_mean ~ gamma(1, 10);
  lambda_rate ~ gamma(1, 0.01);

  etau_mean ~ gamma(5, 0.1);
  etau_rate ~ gamma(1, 1);

  etau   ~ gamma(etau_shape,   etau_rate);
  lambda ~ gamma(lambda_shape, lambda_rate);

  // likelihood
  target += k .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(
      log(lambda[i]) - (lambda[i] + mu[i]) .* T[i],
      log(mu[i]) - (lambda[i] + mu[i]) .* t[i]
      );
  }
}

