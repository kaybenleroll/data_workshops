data {
  int<lower=0> N;               // number of customers

  vector<lower=0>[N] T;         // observation period
  vector<lower=0>[N] recency;   // time between last transaction and observation period end
  vector<lower=0>[N] frequency; // number of transactions
}

parameters {
  vector<lower=0>[N] lambda;   // per-customer transaction rate
  vector<lower=0>[N] mu;       // per-customer churn rate

  // shape and rate underlying lambda
  real<lower=0> lambda_shape;
  real<lower=0> lambda_rate;

  // shape and rate underlying mu
  real<lower=0> mu_shape;
  real<lower=0> mu_rate;
}

model {
  // cache some calculations
  vector[N] lambda_plus_mu = lambda + mu;

  lambda_shape ~ exponential(1);
  lambda_rate  ~ exponential(1);
  mu_shape     ~ exponential(1);
  mu_rate      ~ exponential(1);

  lambda ~ gamma(lambda_shape, lambda_rate);
  mu     ~ gamma(mu_shape, mu_rate);

  // increase log likelihood
  target += frequency .* log(lambda) - log(lambda_plus_mu);
  target += log(mu .* exp(-lambda_plus_mu .* recency) + lambda .* exp(-lambda_plus_mu .* T));
}
