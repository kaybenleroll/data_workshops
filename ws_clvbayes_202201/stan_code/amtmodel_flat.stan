data {
  real<lower=0> p;                // parameter 1 for Gamma top level
  real<lower=0> q;                // parameter q for Gamma nu draw
  real<lower=0> g;                // parameter g for Gamma nu draw

  int<lower=1> n;                    // count of transactions
  int<lower=1> n_customer_id;        // count of customers

  array[n] int<lower=0> customer_id; // index for customer

  vector<lower=0>[n] tnx_amt;        // transaction amount
}

parameters {
  vector<lower=0>[n_customer_id] nu;
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
