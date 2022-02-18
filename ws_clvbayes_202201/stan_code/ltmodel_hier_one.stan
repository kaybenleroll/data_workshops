data {
  int<lower=1> n;               // count of observations

  vector<lower=0>[n] min_lifetime; // minimum observed lifetime for customer
  vector<lower=0>[n] max_lifetime; // maximum observed lifetime for customer

  real<lower=0> mean_p1, mean_p2;
  real<lower=0> s;
}

parameters {
  real<lower=0> hier_mean;

  vector<lower=0>[n] mu;
}

transformed parameters {
  real<lower=0> beta = s / hier_mean;
}

model {
  hier_mean ~ gamma(mean_p1, mean_p1);

  mu ~ gamma(s, beta);

  target += exponential_lccdf(min_lifetime | mu);
  target += exponential_lcdf (max_lifetime | mu);
}

generated quantities {
  vector<lower=0>[n] tau;

  tau = 1 ./ mu;
}
