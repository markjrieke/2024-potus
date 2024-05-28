data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> D;                      // Number of pre-election days

  // Mapping IDs
  array[N] int<lower=0, upper=D> did;  // Map of pre-election day to observation

  // Response data
  vector<lower=-1, upper=1>[N] Y;      // FTE model output

  // Priors over the initial state
  real theta0_mu;                      // Initial state mean
  real<lower=0> theta0_sigma;          // Initial state scale

  // Priors over state/observation
  real<lower=0> sigma_s_alpha;         // Shape for gamma prior over state scale
  real<lower=0> sigma_s_beta;          // Inverse-scale for gamma prior over state scale
  real<lower=0> sigma_o_alpha;         // Shape for gamma prior over observation scale
  real<lower=0> sigma_o_beta;          // Inverse-scale for gamma prior over observation scale

  // Priors over final (e_day) state
  real thetaD_mu;                      // Final state mean
  real thetaD_sigma;                   // Final state scale

  // Debug
  real<lower=0> sigma_o_lim;           // Lower limit for observation scale
  int<lower=0, upper=1> prior_check;
}

parameters {
  real<lower=-1, upper=1> theta0;      // Initial state
  vector[D-1] eta_d;                   // State random walk
  real<lower=0> sigma_s;               // State scale
  real<lower=sigma_o_lim> sigma_o;     // Observation scale
}

transformed parameters {
  // Estimate the state
  vector[D] theta;
  theta[1] = theta0;
  theta[2:D] = sigma_s * eta_d;
  for (d in 2:D) {
    theta[d] += theta[d-1];
  }
}

model {
  // Priors over initial state
  target += normal_lpdf(theta0 | theta0_mu, theta0_sigma);

  // Priors over final state
  target += normal_lpdf(theta[D] | thetaD_mu, thetaD_sigma);

  // Priors over state-space
  target += std_normal_lpdf(eta_d);
  target += gamma_lpdf(sigma_s | sigma_s_alpha, sigma_s_beta);

  // Priors over observation-space
  target += gamma_lpdf(sigma_o | sigma_o_alpha, sigma_o_beta);

  // Likelihood
  if (!prior_check) {
    for (n in 1:N) {
      target += normal_lpdf(Y[n] | theta[did[n]], sigma_o);
    }
  }
}

generated quantities {
  real thetaD = theta[D];
}
