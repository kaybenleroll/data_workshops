functions {
  real growth_factor_weibull(real t, real omega, real theta) {
    real factor;

    factor = 1 - exp(-(t/theta)^omega);

    return(factor);
  }

  real growth_factor_loglogistic(real t, real omega, real theta) {
    real factor;

    factor = ((t^omega) / (t^omega + theta^omega));

    return(factor);
  }
}

data {
  int<lower=0,upper=1> growthmodel_id;

  int n_data;
  int n_cohortdata;
  int n_time;
  int n_cohort;
  int n_org;

  int cohort_id[n_data];
  int org_id[n_data];
  int t_idx[n_data];

  real<lower=0> t_value[n_time];

  real premium[n_data];
  real loss[n_data];

  int cohort_maxtime[n_cohortdata];
}

parameters {
  real<lower=0> omega[n_org];
  real<lower=0> theta[n_org];

  real<lower=0> LR[n_org, n_cohort];

  real          mu_LR[n_org];
  real<lower=0> sd_LR[n_org];

  real<lower=0> loss_sd[n_org];

  real          mu_omega;
  real<lower=0> sd_omega;
  real          mu_theta;
  real<lower=0> sd_theta;

  real          hyper_mu_LR_mean;
  real<lower=0> hyper_mu_LR_sd;

  real          hyper_sd_LR_mean;
  real<lower=0> hyper_sd_LR_sd;

  real          hyper_loss_sd_mean;
  real<lower=0> hyper_loss_sd_sd;
}

transformed parameters {
  real gf[n_org, n_time];
  real loss_mean[n_org, n_cohort, n_time];

  for(i in 1:n_org) {
    for(j in 1:n_time) {
      if(growthmodel_id == 1) {
        gf[i,j] = growth_factor_weibull    (t_value[j], omega[i], theta[i]);
      } else {
        gf[i,j] = growth_factor_loglogistic(t_value[j], omega[i], theta[i]);
      }
    }
  }

  for(i in 1:n_data) {
    loss_mean[org_id[i], cohort_id[i], t_idx[i]] = LR[org_id[i], cohort_id[i]] *
                                                    premium[i] *
                                                    gf[org_id[i], t_idx[i]];
  }
}

model {
  mu_LR ~ normal   (hyper_mu_LR_mean, hyper_mu_LR_sd);
  sd_LR ~ lognormal(hyper_sd_LR_mean, hyper_sd_LR_sd);

  loss_sd ~ lognormal(hyper_loss_sd_mean, hyper_loss_sd_sd);

  omega ~ lognormal(mu_omega, sd_omega);
  theta ~ lognormal(mu_theta, sd_theta);

  mu_omega ~ normal(0, 1);
  sd_omega ~ lognormal(-3, 0.1);
  mu_theta ~ normal(0, 1);
  sd_theta ~ lognormal(-3, 0.1);

  hyper_mu_LR_mean ~ normal(0, 1);
  hyper_mu_LR_sd   ~ lognormal(0, 1);
  hyper_sd_LR_mean ~ normal(0, 1);
  hyper_sd_LR_sd   ~ lognormal(0, 1);

  hyper_loss_sd_mean ~ normal(0, 1);
  hyper_loss_sd_sd   ~ lognormal(0, 0.1);

  for(i in 1:n_data) {
    loss[i] ~ normal(loss_mean[org_id[i], cohort_id[i], t_idx[i]], premium[i] * loss_sd[org_id[i]]);
  }

  for(j in 1:n_org) {
    LR[j] ~ lognormal(mu_LR[j], sd_LR[j]);
  }
}


generated quantities {
  real mu_omega_exp;
  real mu_theta_exp;

  real hyper_mu_LR_mean_exp;
  real hyper_sd_LR_mean_exp;
  real hyper_loss_sd_mean_exp;

  real mu_LR_exp[n_org];

  real ppc_minLR;
  real ppc_maxLR;

  int t_098[n_org];

  mu_omega_exp = exp(mu_omega);
  mu_theta_exp = exp(mu_theta);

  for(j in 1:n_org) {
    mu_LR_exp[j] = exp(mu_LR[j]);
  }

  hyper_mu_LR_mean_exp   = exp(hyper_mu_LR_mean);
  hyper_sd_LR_mean_exp   = exp(hyper_sd_LR_mean);
  hyper_loss_sd_mean_exp = exp(hyper_loss_sd_mean);

  // Create PPC distributions for the max/min of LR
  {
    real tempmin[n_org];
    real tempmax[n_org];

    for(i in 1:n_org) {
      tempmin[i] = min(LR[i]);
      tempmax[i] = max(LR[i]);
    }

    ppc_minLR = min(tempmin);
    ppc_maxLR = max(tempmax);
  }


  // Create PPC distributions the time to 0.99 gf for each org
  for(i in 1:n_org) {
    t_098[i] = 1;

    for(j in 1:n_time) {
      if(gf[i,j] <= 0.98) t_098[i] = j;
    }
  }


}
