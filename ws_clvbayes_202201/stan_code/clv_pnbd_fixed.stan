functions {
  #include util_functions.stan
}

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

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}
