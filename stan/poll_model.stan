functions {
  // @TODO: separate out into own file
  /*
    @TODO: write documentation etc.
    Generate a covariance matrix from a pre-computed distance matrix

  */
  matrix gp_exp_quad_cov(matrix F,
                         real sigma,
                         real rho) {
    int N = dims(F)[1];
    matrix [N,N] K;
    for (i in 1:(N - 1)) {
      K[i,i] = sigma + 1e-9;
      for (j in (i + 1):N) {
        K[i,j] = sigma * exp(-rho * square(F[i,j]));
        K[j,i] = K[i,j];
      }
    }
    K[N,N] = sigma + 1e-9;
    return K;
  }
}

data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> S;                      // Number of states (including composites)
  int<lower=1> S_nc;                   // Number of states (excluding composites)
  int<lower=1> D;                      // Number of observed days (e-day == D)

  // Data for state covariance matrix
  matrix[S_nc, S_nc] F_mat;            // Euclidean distance between states in feature-space

  // Data for creating composite states
  int<lower=1, upper=S> Nat_id;        // State ID for the national composite
  vector<lower=0>[S_nc] Nat_wt;        // Weight of each raw parameter in the national composite

  // Mapping IDs
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll
  array[N] int<lower=1, upper=D> did;  // Map of day to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll
}

transformed data {
  int S_c = S - S_nc;                  // Number of composite states
  array[D] real D_arr = linspaced_array(D, 1, D);
  real rho_d = 90;
  real sigma_d = 0.02;
}

parameters {
  real<lower=0> rho_s;                 // Length-scale parameter for state GP
  real<lower=0> sigma_s;               // Amplitude parameter for state GP
  vector[S_nc] eta_s;                  // Latent function for state GP
  // real<lower=0> rho_d;                 // Length-scale parameter for day GP
  // real<lower=0> sigma_d;               // Amplitude parameter for day GP
  matrix[D, S_nc] eta_sd;              // Latent function for day GP
}

transformed parameters{
  // Construct covariance matrix from feature space
  // @TODO: coalesce into functions
  matrix[S_nc, S_nc] K_s = gp_exp_quad_cov(F_mat, sigma_s, rho_s);
  matrix[S_nc, S_nc] L_s = cholesky_decompose(K_s);
  vector[S_nc] beta_s_nc = L_s * eta_s;

  // Construct covariance matrix over days
  matrix[D, D] K_d = gp_exp_quad_cov(D_arr, sigma_d, rho_d);
  for (d in 1:D) {
    K_d[d,d] += 1e-9;
  }
  matrix[D, D] L_d = cholesky_decompose(K_d);
  matrix[D, S_nc] beta_sd_nc = L_d * eta_sd;

  // Append state parameters with composite states
  vector[S] beta_s = append_row(beta_s_nc, zeros_vector(S_c));
  beta_s[Nat_id] = dot_product(beta_s_nc, Nat_wt);

  // Append day parameters with composite states
  matrix[S, D] beta_sd = rep_matrix(0, S, D);
  beta_sd[1:S_nc, :] = transpose(beta_sd_nc);
  beta_sd[Nat_id, :] = transpose(beta_sd_nc * Nat_wt);

  // Estimate the linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s[sid[n]] + beta_sd[sid[n], did[n]];
  }
}

model {
  // priors
  target += exponential_lpdf(rho_s | 3);
  // target += gamma_lpdf(rho_d | 3, 1.0/15);
  target += gamma_lpdf(sigma_s | 5, 10);
  // target += gamma_lpdf(sigma_d | 5, 10);
  target += normal_lpdf(eta_s | 0, 1);
  target += normal_lpdf(to_vector(eta_sd) | 0, 1);

  // likelihood
  target += binomial_logit_lpmf(Y | K, mu);
}

generated quantities {
  vector[N] theta = inv_logit(mu);
}
