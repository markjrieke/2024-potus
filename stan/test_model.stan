data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> D;                      // Number of observed days (e-day == D)
  int<lower=1> P;                      // Number of pollsters
  int<lower=1> G;                      // Number of groups (populations)
  int<lower=1> M;                      // Number of poll modes

  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll
  array[N] int<lower=1, upper=G> gid;  // Map of group (population) to poll
  array[N] int<lower=1, upper=M> mid;  // Map of mode to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll
}

transformed data {
  array[D] real D_arr = to_array_1d(linspaced_vector(D, 1, D)/D);
}

parameters {
  // Fixed effects
  real beta_s;                         // State-specific parameter
  vector[P] beta_p;                    // Pollster bias
  vector[G] beta_g;                    // Group (population) bias
  vector[M] beta_m;                    // Mode bias
  vector[N] beta_n;                    // Poll-specific bias

  // Gaussian process parameters
  real<lower=0> rho_d;                 // Length-scale parameter for day GP
  real<lower=0> sigma_d;               // Amplitude parameter for day GP
  vector[D] eta_d;
}

transformed parameters{
  // real rho_d = 1.0 * 90/180;
  // real sigma_d = 0.02;
  // Construct covariance matrix over days
  vector[D] beta_d;
  {
    matrix[D, D] K_d = gp_exp_quad_cov(D_arr, sigma_d, rho_d);
    for (d in 1:D) {
      K_d[d,d] += 1e-9;
    }
    matrix[D, D] L_d = cholesky_decompose(K_d);
    beta_d = L_d * eta_d;
  }

  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s + beta_d[did[n]] + beta_p[pid[n]] + beta_g[gid[n]] + beta_m[mid[n]] + beta_n[n];
  }
}

model {
  // priors
  target += normal_lpdf(beta_s | 0, 1);
  target += normal_lpdf(beta_p | 0, 0.05);
  target += normal_lpdf(beta_g | 0, 0.05);
  target += normal_lpdf(beta_m | 0, 0.05);
  target += normal_lpdf(beta_n | 0, 0.05);
  // target += inv_gamma_lpdf(rho_d | 5, 5);
  target += gamma_lpdf(rho_d | 3, 10);
  target += normal_lpdf(sigma_d | 0, 0.05) - normal_lccdf(0 | 0, 0.05);
  target += normal_lpdf(eta_d | 0, 1);

  // likelihood
  target += binomial_logit_lpmf(Y | K, mu);
}

generated quantities {
  vector[D] theta_pred = inv_logit(beta_s + beta_d);
}
