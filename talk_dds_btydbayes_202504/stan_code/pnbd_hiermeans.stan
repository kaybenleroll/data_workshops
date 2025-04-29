functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;              // number of customers

  vector<lower=0>[n] t_x;      // time to most recent purchase
  vector<lower=0>[n] T_cal;    // total observation time
  vector<lower=0>[n] x;        // number of purchases observed

  real<lower=0> lambda_cv;     // prior cv for lambda
  real<lower=0> mu_cv;         // prior cv for mu

  real          lambda_mn_p1;  // hyperprior p1 for lambda mean
  real<lower=0> lambda_mn_p2;  // hyperprior p2 for lambda mean

  real          mu_mn_p1;      // hyperprior p1 for mu mean
  real<lower=0> mu_mn_p2;      // hyperprior p2 for mu mean
}


parameters {
  real<lower=0> lambda_mn;
  real<lower=0> mu_mn;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}

transformed parameters {
  real<lower=0> r     = 1 / (lambda_cv * lambda_cv);
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  real<lower=0> s     = 1 / (mu_cv * mu_cv);
  real<lower=0> beta  = 1 / (mu_cv * mu_cv * mu_mn);
}

model {
  // model the hyper-prior
  lambda_mn ~ lognormal(lambda_mn_p1, lambda_mn_p2);
  mu_mn     ~ lognormal(mu_mn_p1,     mu_mn_p2);

  // setting priors
  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s,  beta);

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}

generated quantities {
  vector[n] p_alive;         // Probability that they are still "alive"

  p_alive = 1 ./ (1 + mu ./ (mu + lambda) .* (exp((lambda + mu) .* (T_cal - t_x)) - 1));
}
