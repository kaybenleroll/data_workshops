data {
  int<lower=1> n;               // count of observations

  int<lower=0> btyd_count[n];   // observed number of transactions
  vector<lower=0>[n] tnx_weeks; // observed time-period of transactions

  real<lower=0> mu_p1, mu_p2;
  real<lower=0> cov_p1, cov_p2;
}

parameters {
  real<lower=0> hier_mu;
  real<lower=0> hier_cov;

  vector<lower=0>[n] lambda;
}

transformed parameters {
  // shape <- 1 / (cv^2)
  // scale <- mu * cv^2

  real<lower=0> r     = 1 / (hier_cov * hier_cov);
  real<lower=0> alpha = 1 / (hier_mu  * hier_cov * hier_cov);
}


model {
  hier_mu  ~ gamma(mu_p1,  mu_p1);
  hier_cov ~ gamma(cov_p1, cov_p2);

  lambda ~ gamma(r, alpha);

  target += poisson_lpmf(btyd_count | lambda .* tnx_weeks);
}

