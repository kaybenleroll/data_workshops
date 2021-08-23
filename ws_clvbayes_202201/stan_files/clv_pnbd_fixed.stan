data {
  int<lower=1> n;       // number of customers

  vector<lower=0>[n] t; // time to most recent purchase
  vector<lower=0>[n] T; // total observation time
  vector<lower=0>[n] k; // number of purchases observed

  real<lower=0> etau_shape;
  real<lower=0> etau_rate;

  real<lower=0> lambda_shape;
  real<lower=0> lambda_rate;
}


parameters {
  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] etau;   // expected mean lifetime
}


transformed parameters {
  vector<lower=0>[n] mu = 1.0 ./ etau;
}


model {
  // setting priors
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

