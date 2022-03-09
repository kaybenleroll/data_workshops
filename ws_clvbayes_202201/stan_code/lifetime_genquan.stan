vector<lower=0>[n] tau;

vector<lower=0,upper=1>[n] p_alive;
vector<lower=0,upper=1>[n] q_min;

vector[n] log_lik;


// Invert mu to obtain mean expected lifetime tau
tau = 1 ./ mu;

// Calculate probability of customer still being alive at observation time
{
  vector[n] qval2;

  for(i in 1:n) {
    q_min[i] = exponential_cdf(min_lifetime[i] | mu[i]);
    qval2[i] = exponential_cdf(obs_time[i]     | mu[i]);

    log_lik[i] = exponential_lcdf(min_lifetime[i] | mu[i]);
  }

  p_alive = (1 - qval2) ./ (1 - q_min);
}
