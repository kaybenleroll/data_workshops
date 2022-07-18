data {
  real<lower=0> p;                   // parameter 1 for Gamma top level
  real<lower=0> nu_mean;             // parameter for mean of Gamma nu draw
  real<lower=0> nu_cv;               // parameter for cov of Gamma nu draw

  int<lower=1> n;                    // count of transactions
  int<lower=1> n_customer_id;        // count of customers

  array[n] int<lower=0> customer_id; // index for customer

  vector<lower=0>[n] tnx_amt;        // transaction amount
}


transformed data {
  real<lower=0> q = 1 / (nu_cv * nu_cv);
  real<lower=0> g = 1 / (nu_cv * nu_cv * nu_mean);
}


parameters {
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
