data {
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> r;          // hyperprior scale parameter for lambda
  real<lower=0> alpha;      // hyperprior rate parameter for lambda

  real<lower=0> s;          // hyperprior scale parameter for mu
  real<lower=0> beta;       // hyperprior rate parameter for mu
}


parameters {
  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}


model {
  // setting priors
  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s,  beta);

  // likelihood
  target += x .* log(lambda) - log(lambda + mu);

  for (i in 1:n) {
    target += log_sum_exp(
      log(lambda[i]) - (lambda[i] + mu[i]) .* T_cal[i],
      log(mu[i]) - (lambda[i] + mu[i]) .* t_x[i]
      );
  }
}
