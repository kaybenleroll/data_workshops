functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> lambda_mn;  // prior mean for lambda
  real<lower=0> lambda_cv;  // prior cv   for lambda

  real<lower=0,upper=1> p_mn; // prior mean     for p
  real<lower=0>         p_k;  // prior strength for p
}


transformed data {
  real<lower=0> r     = 1 / (lambda_cv * lambda_cv);
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  real<lower=0> a     = p_k * p_mn;
  real<lower=0> b     = p_k * (1 - p_mn);
}


parameters {
  vector<lower=0>[n]         lambda;  // purchase rate
  vector<lower=0,upper=1>[n] p;       // dropout probabilit
}


model {
  // setting priors
  lambda ~ gamma(r, alpha);
  p      ~ beta (a, b);

  target += calculate_bgnbd_loglik(n, lambda, p, x, t_x, T_cal);
}
