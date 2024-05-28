data {
  int<lower=0> N;                 // Number of observations
  vector<lower=0, upper=1>[N] V;  // Incumbent party two-party voteshare
  vector[N] G;                    // 2nd quarter real gdp growth
  vector[N] I;                    // Whether or not the incumbent is running

  // Historical data w/measurement error
  vector[N] A_mu;                 // Approval mean
  vector<lower=0>[N] A_sigma;     // Approval Scale

  // Model Priors
  real alpha_mu;                  // Prior intercept mean
  real<lower=0> alpha_sigma;      // Prior intercept scale
  real beta_a_mu;                 // Prior approval parameter mean
  real<lower=0> beta_a_sigma;     // Prior approval parameter scale
  real beta_g_mu;                 // Prior GDP parameter mean
  real<lower=0> beta_g_sigma;     // Prior GDP parameter scale
  real beta_i_mu;                 // Prior incumbency parameter mean
  real<lower=0> beta_i_sigma;     // Prior incumbency parameter scale
  real<lower=0> sigma_sigma;      // Scale for half-normal prior over observation scale

  // 2024 National Prior
  real A_new_mu;                  // Biden Net Approval Mean
  real A_new_sigma;               // Biden Net Approval Scale
  real G_new;                     // 2024 Real 2nd Quarter GDP Growth
  real I_new;                     // Incumbency Status

  // 2024 State Priors
  int<lower=0> S;                 // Number of States
  vector[S] e_day_mu;             // Mean state-level predicted partisan lean
  vector<lower=0>[S] e_day_sigma; // Standard deviation of state-level predicted partisan lean
}

parameters {
  // Linear Model Parameters
  real alpha;
  real beta_a;
  real beta_g;
  real beta_i;
  real<lower=0> sigma;

  // Measurement Error Data
  vector[N] A;
}

transformed parameters {
  vector<lower=0, upper=1>[N] mu;
  mu = inv_logit(alpha + beta_a * A + beta_g * G + beta_i * I);
}

model {
  // Priors over linear model parameters
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += normal_lpdf(beta_a | beta_a_mu, beta_a_sigma);
  target += normal_lpdf(beta_g | beta_g_mu, beta_g_sigma);
  target += normal_lpdf(beta_i | beta_i_mu, beta_i_sigma);
  target += normal_lpdf(sigma | 0, sigma_sigma) - normal_lccdf(0 | 0, sigma_sigma);

  // Priors over measurement error data
  target += normal_lpdf(A | A_mu, A_sigma);

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
  real A_new = normal_rng(A_new_mu, A_new_sigma);
  real mu_nat = inv_logit(alpha + beta_a * A_new + beta_g * G_new + beta_i * I_new);
  real theta_nat = normal_rng(mu_nat, sigma);

  // State Priors for 2024
  vector[S] mu_state = mu_nat + e_day_mu;
  array[S] real theta_state = normal_rng(mu_state, e_day_sigma);
}

