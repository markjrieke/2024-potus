data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> C;                      // Number of election cycles
  int<lower=1> D;                      // Number of pre-election days

  // Mapping IDs
  array[N] int<lower=1, upper=C> cid;  // Map of election cycle to observation
  array[N] int<lower=1, upper=D> did;  // Map of pre-election day to observation

  // Response data
  vector<lower=-1, upper=1>[N] Y;      // FTE model output

  // Priors
  real<lower=0> sigma_c_sigma;         // Scale for half-normal prior over inital state scale
  real<lower=0> sigma_cd_sigma;        // Scale for half-normal prior over random walk scale
  real<lower=0> sigma_o_sigma;         // Scale for half-normal prior over observation scale

  // Debug
  int<lower=0, upper=1> prior_check;
}

parameters {
  // Initial state parameters
  vector[C] eta_c;
  real<lower=0> sigma_c;

  // State-space parameters
  matrix[C, D] eta_cd;
  vector<lower=0>[C] sigma_cd;

  // Observation-space parameters
  vector<lower=0>[C] sigma_o;
}

transformed parameters {
  // Initial state
  vector[C] beta_c = eta_c * sigma_c;

  // Random walk from initial state
  matrix[C, D] beta_cd = diag_pre_multiply(sigma_cd, eta_cd);
  for (d in 1:(D-1)) {
    beta_cd[:,d+1] += beta_cd[:,d];
  }

  // Construct state-space model
  vector[N] mu;
  vector[N] sigma;
  for (n in 1:N) {
    mu[n] = beta_c[cid[n]] + beta_cd[cid[n], did[n]];
    sigma[n] = sigma_o[cid[n]];
  }
}

model {
  // Priors over initial state
  target += std_normal_lpdf(eta_c);
  target += normal_lpdf(sigma_c | 0, sigma_c_sigma) - C * normal_lccdf(0 | 0, sigma_c_sigma);

  // Priors over random walk
  target += std_normal_lpdf(to_vector(eta_cd));
  target += normal_lpdf(sigma_cd | 0, sigma_cd_sigma) - C * normal_lccdf(0 | 0, sigma_cd_sigma);

  // Priors over observation space
  target += normal_lpdf(sigma_o | 0, sigma_o_sigma) - C * normal_lccdf(0 | 0, sigma_o_sigma);

  // Likelihood
  if (!prior_check) {
    target += normal_lpdf(Y | mu, sigma);
  }
}

generated quantities {
  // Estimated state
  array[C] vector[D] theta;
  for (c in 1:C) {
    theta[c] = beta_c[c] + to_vector(beta_cd[c, :]);
  }
}
