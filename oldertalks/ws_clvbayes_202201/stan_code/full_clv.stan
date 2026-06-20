// Likelihood function
functions {
  vector llh(vector x, vector t_x, vector t_cal, vector lambda1, vector mu1,
             vector log_lambda, vector log_mu, vector log_mu_lambda, int N) {

    vector[N] p_1;
    vector[N] p_2;

    p_1 = x .* log_lambda + log_mu - log_mu_lambda - t_x .* mu1 - t_x .* lambda1;
    p_2 = x .* log_lambda + log_lambda - log_mu_lambda - t_cal .* mu1 - t_cal .* lambda1;


    return(log(exp(p_1) + exp(p_2)));
  }
}


data {
  int<lower=1> N;            // Number of customers
  int<lower=0> N_months;     // Number of months for ltv calibration
  vector<lower=0>[N] x;      // Repeat transactions per customer (frequency)
  vector<lower=0>[N] t_x;    // Time of most recent transation (recency)
  vector<lower=0>[N] t_cal;  // Time since first transaction
  vector[N] mx;              // Average transaction amount
}


transformed data {
  vector<lower=0>[N] x_tot;  // Total number of transactions per cust
  x_tot = x + 1;
}


parameters {
  vector<lower=0>[N] lambda; // Transaction rate
  vector<lower=0>[N] mu;     // Dropout probability

  real<lower=0> rr;          // Transaction shape parameter
  real<lower=0> alpha;       // Transaction scale parameter
  real<lower=0> ss;          // Dropout shape parameter
  real<lower=0> beta;        // Dropout scale parameter
  real<lower=0> p;           // Shape of trans amt gamma

  vector<lower=0>[N] v;      // Scale of trans amt gamma (cust specific)

  real <lower=0> q;          // Shape of scale dist gamma
  real <lower=0> y;          // Scale of scale dist gamma
}


transformed parameters {
  vector[N] log_lambda;
  vector[N] log_mu;
  vector[N] log_mu_lambda;
  vector[N] log_lik;

  vector<lower=0> [N] px;    // Shape of total spend distribution
  vector<lower=0> [N] nx;    // Scale of total spend distribution

  px = p *  x_tot;
  nx = v .* x_tot;

  log_lambda    = log(lambda);
  log_mu        = log(mu);
  log_mu_lambda = log(mu + lambda);

  log_lik = llh(x, t_x, t_cal, lambda, mu, log_lambda, log_mu, log_mu_lambda, N);
}


model {
  // Priors for rates
  lambda ~ gamma(rr, alpha);
  mu     ~ gamma(ss, beta);
  rr     ~ exponential(1);
  alpha  ~ exponential(1);
  ss     ~ exponential(0.1);
  beta   ~ exponential(0.1);

  // Likelihood for rate
  target += log_lik;

  // Priors for spend
  p ~ exponential(0.1);
  q ~ exponential(0.1);
  y ~ exponential(0.1);
  v ~ gamma(q, y);

  // Likelihood for spend
  mx ~ gamma(px, nx);
}


generated quantities {
  vector[N] p_alive;         // Probability that they are still "alive"
  vector[N] exp_trans;       // Expected number of transactions
  vector[N] mx_pred;         // Per transaction spend
  vector[N] lt_val;          // Lifetime value

  for(i in 1:N) {
    p_alive[i]   = 1/(1+mu[i]/(mu[i]+lambda[i])*(exp((lambda[i]+mu[i])*(t_cal[i]-t_x[i]))-1));
    exp_trans[i] = (lambda[i]/mu[i])*(1 - exp(-mu[i]*N_months));
    mx_pred[i]   = gamma_rng(px[i], nx[i]);
  }

  lt_val = exp_trans .* mx_pred;
}
