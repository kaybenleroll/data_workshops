functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real          lambda_mn_p1;  // lambda mean prior p1
  real<lower=0> lambda_mn_p2;  // lambda mean prior p2

  real<lower=0> lambda_cv;

  real<lower=0> p_mn_mu;       // p mean prior p1
  real<lower=0> p_mn_k;        // p mean prior p2

  real<lower=0> p_k;
}


transformed data {
  real<lower=0> r = 1 / (lambda_cv * lambda_cv);

  real<lower=0> p_mn_a = p_mn_k * p_mn_mu;
  real<lower=0> p_mn_b = p_mn_k * (1 - p_mn_mu);
}

parameters {
  real<lower=0>              lambda_mn;
  real<lower=0,upper=1>      p_mn;

  vector<lower=0>[n]         lambda;  // purchase rate
  vector<lower=0,upper=1>[n] p;       // dropout probabilit
}


transformed parameters {
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  real<lower=0> p_a   = p_k * p_mn;
  real<lower=0> p_b   = p_k * (1 - p_mn);
}


model {
  // set hyper-priors
  lambda_mn ~ lognormal(lambda_mn_p1, lambda_mn_p2);

  p_mn      ~ beta     (p_mn_a, p_mn_b);

  // setting priors
  lambda ~ gamma(r,   alpha);
  p      ~ beta (p_a, p_b);

  target += calculate_bgnbd_loglik(n, lambda, p, x, t_x, T_cal);
}
