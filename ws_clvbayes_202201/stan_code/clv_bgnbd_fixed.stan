data {
  int<lower=1> n;           // number of customers

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> r;          // hyperprior scale parameter for lambda
  real<lower=0> alpha;      // hyperprior rate parameter for lambda

  real<lower=0> a;          // hyperprior shape1 parameter for p
  real<lower=0> b;          // hyperprior shape2 parameter for p
}


parameters {
  vector<lower=0>[n]         lambda;  // purchase rate
  vector<lower=0,upper=1>[n] p;       // dropout probabilit
}


model {
  // setting priors
  lambda ~ gamma(r, alpha);
  p      ~ beta (a, b);

  vector[n] t1;
  vector[n] t2;

  // likelihood
  t1 = log(p) + (x-1) .* log(1-p) + x .* log(lambda) - lambda .* t_x;
  t2 = x .* log(1-p) + x .* log(lambda) - lambda .* T_cal;

  for(i in 1:n) {
    target += log_sum_exp(t1[i], t2[i]);
  }
}
