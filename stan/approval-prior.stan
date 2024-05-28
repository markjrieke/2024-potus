data {
  // Dimensions of the dataset
  int<lower=0> N;                        // Number of observations
  int<lower=1> C;                        // Number of election cycles
  int<lower=1> D;                        // Number of pre-election days

  // Mapping IDs
  array[N] int<lower=0, upper=C> cid;    // Map of election cycle to observation
  array[N] int<lower=0, upper=D> did;    // Map of pre-election day to observation

  // Response data
  vector<lower=-1, upper=1>[N] Y;        // FTE model output

  // Priors over the initial state
  real theta0_mu;                        // Initial state mean
  real<lower=0> theta0_sigma;            // Initial state scale

  // Priors over state/observation
  real<lower=0> sigma_s_alpha;           // Shape for gamma prior over state scale
  real<lower=0> sigma_s_beta;            // Inverse-scale for gamma prior over state scale
  real<lower=0> sigma_o_alpha;           // Shape for gamma prior over observation scale
  real<lower=0> sigma_o_beta;            // Inverse-scale for gamma prior over observation scale

  // Debug
  real<lower=0> sigma_o_lim;             // Lower limit for observation scale
  int<lower=0, upper=1> prior_check;
}

parameters {
  vector<lower=-1, upper=1>[C] theta0;   // Initial state
  matrix[C, D-1] eta_cd;                 // State random walk
  vector<lower=0>[C] sigma_s;            // State scale
  vector<lower=sigma_o_lim>[C] sigma_o;  // Observation scale
}

transformed parameters {
  // Estimate the state
  matrix[C, D] theta;
  theta[:,1] = theta0;
  theta[:,2:D] = diag_pre_multiply(sigma_s, eta_cd);
  for (d in 2:D) {
    theta[:,d] += theta[:,d-1];
  }
}

model {
  // Priors over initial state
  target += normal_lpdf(theta0 | theta0_mu, theta0_sigma);

  // Priors over state-space
  target += std_normal_lpdf(to_vector(eta_cd));
  target += gamma_lpdf(sigma_s | sigma_s_alpha, sigma_s_beta);

  // Priors over observation-space
  target += gamma_lpdf(sigma_o | sigma_o_alpha, sigma_o_beta);

  // Likelihood
  if (!prior_check) {
    for (n in 1:N) {
      target += normal_lpdf(Y[n] | theta[cid[n], did[n]], sigma_o[cid[n]]);
    }
  }
}

generated quantities {
  // Priors over change in approval
  real delta_mu = mean(theta[:,D] - theta[:,1]);
  real delta_sd = sd(theta[:, D] - theta[:,1]);
  real delta = normal_rng(delta_mu, delta_sd);

  // Priors over historical approval
  vector[C] thetaD = theta[:,D];
}
