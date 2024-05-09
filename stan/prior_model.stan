data {
  int<lower=0> N;                 // Number of observations
  vector<lower=0, upper=1>[N] V;  // Incumbent party two-party voteshare
  vector[N] A;                    // Incumbent net approval
  vector[N] G;                    // 2nd quarter real gdp growth
  vector[N] I;                    // Whether or not the incumbent is running

  // 2024 National Prior
  real A_new;                     // Biden Net Approval
  real G_new;                     // 2024 Real 2nd Quarter GDP Growth
  real I_new;                     // Incumbency Status

  // 2024 State Priors
  int<lower=0> S;                 // Number of States
  vector[S] e_day_mu;             // Mean state-level predicted partisan lean
  vector<lower=0>[S] e_day_sigma; // Standard deviation of state-level predicted partisan lean
}

parameters {
  real alpha;
  real beta_a;
  real beta_g;
  real beta_i;
  real<lower=0> sigma;
}

transformed parameters {
  vector<lower=0, upper=1>[N] mu;
  mu = inv_logit(alpha + beta_a * A + beta_g * G + beta_i * I);
}

model {
  // priors
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(beta_a | 0, 1);
  target += normal_lpdf(beta_g | 0, 1);
  target += normal_lpdf(beta_i | 0, 1);
  target += normal_lpdf(sigma | 0, 1) - normal_lccdf(0 | 0, 1);

  // likelihood
  target += normal_lpdf(V | mu, sigma);
}

generated quantities {
  // Posterior Predictive
  array[N] real y_rep = normal_rng(mu, sigma);
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(V[n] | mu[n], sigma);
  }

  // National Prior for 2024
  real mu_nat = inv_logit(alpha + beta_a * A_new + beta_g * G_new + beta_i * I_new);
  real theta_nat = normal_rng(mu_nat, sigma);

  // State Priors for 2024
  vector[S] mu_state = mu_nat + e_day_mu;
  array[S] real theta_state = normal_rng(mu_state, e_day_sigma);
}

