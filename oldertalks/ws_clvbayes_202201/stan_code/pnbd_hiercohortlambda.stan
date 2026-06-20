functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;             // number of customers
  int<lower=1> n_cohort;

  array[n] int<lower=1,upper=n_cohort> cohort;

  vector<lower=0>[n] t_x;     // time to most recent purchase
  vector<lower=0>[n] T_cal;   // total observation time
  vector<lower=0>[n] x;       // number of purchases observed

  real<lower=0> lambda_cv;    // prior cv   for lambda
  real<lower=0> mu_mn;        // prior mean for mu
  real<lower=0> mu_cv;        // prior cv   for mu

  real          lambda_mn_p1; // hyperprior p1 for lambda mean
  real<lower=0> lambda_mn_p2; // hyperprior p2 for lambda mean
}

transformed data {
  real<lower=0> s    = 1 / (mu_cv * mu_cv);
  real<lower=0> beta = 1 / (mu_cv * mu_cv * mu_mn);
}


parameters {
  vector<lower=0>[n_cohort] lambda_mn;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}


transformed parameters {
  real<lower=0>             r;
  vector<lower=0>[n_cohort] alpha;

  r     = 1  / (lambda_cv  * lambda_cv);
  alpha = 1 ./ (lambda_cv .* lambda_cv .* lambda_mn);
}

model {
  // model the hyper-prior
  lambda_mn ~ lognormal(lambda_mn_p1, lambda_mn_p2);

  // setting priors
  lambda ~ gamma(r, alpha[cohort]);
  mu     ~ gamma(s,          beta);

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}

generated quantities {
  vector[n] mpl;             // intermediate value for efficiency
  vector[n] p_alive;         // Probability that they are still "alive"

  mpl     = mu + lambda;
  p_alive = 1 ./ (1 + mu ./ mpl .* (exp(mpl .* (T_cal - t_x)) - 1));
}
