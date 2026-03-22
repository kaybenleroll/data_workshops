//
// This Stan program defines a simple model for determining the expected return
// and volatility of an asset class.

// The input data is a vector 'y' of length 'N' - consisting of the returns
data {
  int<lower=0,upper=1> prior_pd;

  int<lower=0> N;
  vector[N] y;

  real          return_prior_mu;
  real<lower=0> return_prior_sd;
  real          vol_prior_mu;
  real<lower=0> vol_prior_sd;
}

// The parameters accepted by the model. Our model accepts two parameters
// 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output 'y' to be normally
// distributed with mean 'mu' and standard deviation 'sigma'. We set our priors
// accordingly.

model {
  mu    ~ normal(return_prior_mu, return_prior_sd);
  sigma ~ lognormal(vol_prior_mu, vol_prior_sd);

  if(prior_pd == 1) {
    y ~ normal(mu, sigma);
  }
}

/// We want to generate some data to help us do posterior predictive checks
generated quantities {
  real y_sim;

  y_sim = normal_rng(mu, sigma);
}
