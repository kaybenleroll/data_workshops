functions {
  #include util_functions.stan
}

data {
  int<lower=1> n;              // number of customers

  vector<lower=0>[n] t_x;      // time to most recent purchase
  vector<lower=0>[n] T_cal;    // total observation time
  vector<lower=0>[n] x;        // number of purchases observed

  real          r_p1;          // Prior parameter p1 for r;
  real<lower=0> r_p2;          // Prior parameter p2 for r;

  real          alpha_p1;      // Prior parameter p1 for alpha;
  real<lower=0> alpha_p2;      // Prior parameter p2 for alpha;

  real          s_p1;          // Prior parameter p1 for s;
  real<lower=0> s_p2;          // Prior parameter p2 for s;

  real          beta_p1;       // Prior parameter p1 for beta;
  real<lower=0> beta_p2;       // Prior parameter p2 for beta;
}


parameters {
  real<lower=0> r;
  real<lower=0> alpha;

  real<lower=0> s;
  real<lower=0> beta;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}

model {
  // Setting the priors on the four parameters
  r     ~ lognormal(r_p1,     r_p2);
  alpha ~ lognormal(alpha_p1, alpha_p2);

  s     ~ lognormal(s_p1,     s_p2);
  beta  ~ lognormal(beta_p1,  beta_p2);

  // setting lambda and mu
  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s,  beta);

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}

generated quantities {
  vector[n] p_alive;         // Probability that they are still "alive"

  p_alive = 1 ./ (1 + mu ./ (mu + lambda) .* (exp((lambda + mu) .* (T_cal - t_x)) - 1));
}
