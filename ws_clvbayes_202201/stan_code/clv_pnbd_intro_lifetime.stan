data {
  int<lower=1> n;       // number of customers

  vector<lower=0>[n] t; // time to most recent purchase
  vector<lower=0>[n] T; // total observation time
  vector<lower=0>[n] k; // number of purchases observed

  real<lower=0> etau_shape;
  real<lower=0> etau_rate;
}


parameters {
  vector<lower=0>[n] etau;   // expected mean lifetime
}


transformed parameters {
  vector<lower=0>[n] mu = 1.0 ./ etau;
}


model {
  // setting priors
  etau ~ gamma(etau_shape, etau_rate);

  // likelihood
  target += exponential_lccdf(t | mu);
}
