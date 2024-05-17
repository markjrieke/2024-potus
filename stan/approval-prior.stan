data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> C;                      // Number of election cycles
  int<lower=1> D;                      // Number of pre-election days

  // Mapping IDs
  array[N] int<lower=0, upper=C> cid;  // Map of election cycle to observation
  array[N] int<lower=0, upper=D> did;  // Map of pre-election day to observation

  // Response data
  vector<lower=-1, upper=1>[N] Y;      // FTE model output

  // Priors over the initial state
  real m0;                             // Initial state mean
  real<lower=0> C0;                    // Initial state scale

  // Priors over state/observation
  real<lower=0> W_alpha;               // Shape for gamma prior over state scale
  real<lower=0> W_beta;                // Inverse-scale for gamma prior over state scale
  real<lower=0> V_alpha;               // Shape for gamma prior over observation scale
  real<lower=0> V_beta;                // Inverse-scale for gamma prior over observation scale

  // Debug
  real<lower=0> V_L;                   // Lower limit for observation scale
  int<lower=0, upper=1> prior_check;
}

parameters {
  vector<lower=-1, upper=1>[C] theta0; // Initial state
  matrix[C, D-1] eta_cd;               // State random walk
  vector<lower=0>[C] W;                // State scale
  vector<lower=V_L>[C] V;              // Observation scale
}

transformed parameters {
  // Estimate the state
  matrix[C, D] theta;
  theta[:,1] = theta0;
  theta[:,2:D] = diag_pre_multiply(W, eta_cd);
  for (d in 2:D) {
    theta[:,d] += theta[:,d-1];
  }
}

model {
  // Priors over initial state
  target += normal_lpdf(theta0 | m0, C0);

  // Priors over state-space
  target += std_normal_lpdf(to_vector(eta_cd));
  target += gamma_lpdf(W | W_alpha, W_beta);

  // Priors over observation-space
  target += gamma_lpdf(V | V_alpha, V_beta);

  // Likelihood
  if (!prior_check) {
    for (n in 1:N) {
      target += normal_lpdf(Y[n] | theta[cid[n], did[n]], V[cid[n]]);
    }
  }
}

generated quantities {
  real delta = mean(theta[:,D] - theta[:,1]);
}
