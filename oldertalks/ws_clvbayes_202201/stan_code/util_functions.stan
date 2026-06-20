real calculate_pnbd_loglik(int n, vector lambda, vector mu,
                           data vector x, data vector t_x, data vector T_cal) {
  // likelihood
  vector[n] t1;
  vector[n] t2;

  vector[n] lpm;
  vector[n] lht;
  vector[n] rht;

  lpm = lambda + mu;

  lht = log(lambda) - lpm .* T_cal;
  rht = log(mu)     - lpm .* t_x;

  t1  = x .* log(lambda) - log(lpm);

  for (i in 1:n) {
    t2[i] = log_sum_exp(lht[i], rht[i]);
  }

  return(sum(t1) + sum(t2));
}


real calculate_bgnbd_loglik(int n, vector lambda, vector p,
                            data vector x, data vector t_x, data vector T_cal) {
  // likelihood
  vector[n] t1;
  vector[n] t2;

  vector[n] lht;
  vector[n] rht;

  lht = log(p) + (x-1) .* log(1-p) + x .* log(lambda) - lambda .* t_x;
  rht = x .* log(1-p) + x .* log(lambda) - lambda .* T_cal;

  for(i in 1:n) {
    t2[i] = log_sum_exp(lht[i], rht[i]);
  }

  return(sum(t2));
}

