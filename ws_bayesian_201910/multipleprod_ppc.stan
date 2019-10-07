data {
  int n_cats;
  int n_trials;

  int trials[n_trials];
  int success[n_trials];
  int cat_id[n_trials];

  real priorShape;
  real priorRate;

  real priorA;
  real priorB;
}


parameters {
  real<lower=0,upper=1> mu[n_cats];
  real<lower=0> K[n_cats];

  real<lower=0,upper=1> theta[n_trials];
}


transformed parameters {
  real a[n_cats];
  real b[n_cats];

  for(j in 1:n_cats) {
    a[j] = mu[j] * K[j];
    b[j] = (1.0 - mu[j]) * K[j];
  }
}


model {
  for(i in 1:n_trials) {
    success[i] ~ binomial(trials[i], theta[i]);
    theta[i]   ~ beta(a[cat_id[i]], b[cat_id[i]]);
  }

  for(j in 1:n_cats) {
    mu[j] ~ beta(priorA, priorB);
    K[j]  ~ gamma(priorShape, priorRate);
  }
}


generated quantities {


}
