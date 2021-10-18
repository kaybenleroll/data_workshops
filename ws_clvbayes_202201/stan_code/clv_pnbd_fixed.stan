data {
  int<lower=1> n;       // number of customers

  vector<lower=0>[n] t.x;   // time to most recent purchase
  vector<lower=0>[n] T.cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> mu_shape;
  real<lower=0> mu_rate;

  real<lower=0> lambda_shape;
  real<lower=0> lambda_rate;
}


parameters {
  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}


model {
  // setting priors
  mu     ~ gamma(mu_shape,     mu_rate);
  lambda ~ gamma(lambda_shape, lambda_rate);

  // likelihood
  target += x .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(
      log(lambda[i]) - (lambda[i] + mu[i]) .* T.cal[i],
      log(mu[i]) - (lambda[i] + mu[i]) .* t.x[i]
      );
  }
}

