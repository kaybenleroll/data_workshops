data {
  real<lower=0> p_p1;                // parameter 1 for Gamma p prior
  real<lower=0> p_p2;                // parameter 2 for Gamma p prior

  real<lower=0> nu_p1;               // parameter 1 for Gamma nu prior
  real<lower=0> nu_p2;               // parameter 2 for Gamma nu prior

  int<lower=1> n;                    // count of transactions
  int<lower=1> n_customer_id;        // count of customers

  array[n] int<lower=0> customer_id; // index for customer

  vector<lower=0>[n] tnx_amt;        // transaction amount
}

parameters {
  vector<lower=0>[n_customer_id] p;
  vector<lower=0>[n_customer_id] nu;
}

model {
  p  ~ gamma(p_p1,  p_p2);
  nu ~ gamma(nu_p1, nu_p2);

  for(i in 1:n) {
    tnx_amt[i] ~ gamma(p[customer_id[i]], nu[customer_id[i]]);
  }
}

generated quantities {
  vector[n] log_lik;

  for (i in 1:n) {
    log_lik[i] = gamma_lpdf(tnx_amt[i] | p[customer_id[i]], nu[customer_id[i]]);
  }
}
