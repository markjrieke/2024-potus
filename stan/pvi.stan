data {
  // Observations
  int<lower=0> N;               // Number of observations
  vector[N] P;                  // Observed partisan lean of state
  vector[N] C;                  // CPVI estimated partisan lean

  // Priors
  real alpha_mu;                // Location for prior over intercept
  real<lower=0> alpha_sigma;    // Scale for prior over intercept
  real beta_mu;                 // Location for prior over slope
  real<lower=0> beta_sigma;     // Scale for prior over slope
  real<lower=0> sigma_sigma;    // Scale for half-normal prior for standard deviation

  // Generated Quantities
  int<lower=0> S;               // Number of new state observations to generate
  vector[S] C_hat;              // CPVI estimated partisan lean for the new observations
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] mu = alpha + beta * C;
}

model {
  // priors
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += normal_lpdf(beta | beta_mu, beta_sigma);
  target += normal_lpdf(sigma | 0, sigma_sigma) - normal_lccdf(0 | 0, sigma_sigma);

  // likelihood
  target += normal_lpdf(P | mu, sigma);
}

generated quantities {
  array[S] real P_hat = normal_rng(alpha + beta * C_hat, sigma);
}
