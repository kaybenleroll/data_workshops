data {
  real<lower=0> p_mean;           // parameter 1 for Gamma hierarchy for p
  real<lower=0> p_cv;             // parameter 2 for Gamma hierarchy for p

  real<lower=0> nu_mean;          // parameter for mean of Gamma nu draw
  real<lower=0> nu_cv;            // parameter for cov of Gamma nu draw

  int<lower=1> n;                    // count of transactions
  int<lower=1> n_customer_id;        // count of customers

  array[n] int<lower=0> customer_id; // index for customer

  vector<lower=0>[n] tnx_amt;        // transaction amount
}

transformed data {
  // shape <- 1 / (cv^2)
  // rate  <- 1 / mu * cv^2

  real<lower=0> p_shape = 1 / (p_cv * p_cv);
  real<lower=0> p_rate  = 1 / (p_cv * p_cv * p_mean);

  real<lower=0> q = 1 / (nu_cv * nu_cv);
  real<lower=0> g = 1 / (nu_cv * nu_cv * nu_mean);
}


parameters {
  real<lower=0> p;

  vector<lower=0>[n_customer_id] nu;
}


transformed parameters {
  vector<lower=0>[n_customer_id] cust_mean = (p / nu);
  vector<lower=0>[n_customer_id] cust_cv;

  for(i in 1:n_customer_id) {
    cust_cv[i] = sqrt(p);
  }
}


model {
  p ~ gamma(p_shape, p_rate);
  nu ~ gamma(q, g);

  for(i in 1:n) {
    tnx_amt[i] ~ gamma(p, nu[customer_id[i]]);
  }
}

generated quantities {
  vector[n] log_lik;

  for (i in 1:n) {
    log_lik[i] = gamma_lpdf(tnx_amt[i] | p, nu[customer_id[i]]);
  }
}
