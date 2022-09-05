functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> lambda_mn;  // hyperprior mean for lambda
  real<lower=0> lambda_cv;  // hyperprior cv   for lambda

  real<lower=0> mu_mn;      // hyperprior mean for mu
  real<lower=0> mu_cv;      // hyperprior mean for mu
}

transformed data {
  real<lower=0> r     = 1 / (lambda_cv * lambda_cv);
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  real<lower=0> s    = 1 / (mu_cv * mu_cv);
  real<lower=0> beta = 1 / (mu_cv * mu_cv * mu_mn);
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

generated quantities {
  vector[n] p_alive;         // Probability that they are still "alive"

  p_alive = 1 ./ (1 + mu ./ (mu + lambda) .* (exp((lambda + mu) .* (T_cal - t_x)) - 1));
}
