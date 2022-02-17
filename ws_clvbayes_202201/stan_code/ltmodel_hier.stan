data {
  int<lower=1> n;               // count of observations

  vector<lower=0>[n] min_lifetime; // minimum observed lifetime for customer
  vector<lower=0>[n] max_lifetime; // maximum observed lifetime for customer

  real<lower=0> mean_p1, mean_p2;
  real<lower=0> cov_p1, cov_p2;
}

parameters {
  real<lower=0> hier_mean;
  real<lower=0> hier_cov;

  vector<lower=0>[n] mu;
}

transformed parameters {
  // shape <- 1 / (cv^2)
  // scale <- mu * cv^2

  real<lower=0> s    = 1 / (hier_cov  * hier_cov);
  real<lower=0> beta = 1 / (hier_mean * hier_cov * hier_cov);
}

model {
  hier_mean ~ gamma(mean_p1, mean_p1);
  hier_cov  ~ gamma(cov_p1,  cov_p2);

  mu ~ gamma(s, beta);

  target += exponential_lccdf(min_lifetime | mu);
  target += exponential_lcdf (max_lifetime | mu);
}

generated quantities {
  vector<lower=0>[n] tau;

  tau = 1 ./ mu;
}
