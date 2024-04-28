functions {
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
  int<lower=1> D;                      // Number of days
  int<lower=1> S;                      // Number of states

  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll

  // State feature matrix
  matrix[S, S] F_s;                    // State distance matrix in feature space

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // blegh
  vector[S] e_day_mu;
  vector[S] e_day_sigma;
  real phi_sigma;
  real<lower=0> rho_alpha;
  real<lower=0> rho_beta;
  real<lower=0> alpha_sigma;

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  // array[D] real D_arr = to_array_1d(linspaced_vector(D, 1, D)/D);
}

parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  vector[S] eta_s;
  matrix[S,D] eta_d;
  real<lower=0> phi;
}

transformed parameters{
  // Construct covariance matrix from feature space
  matrix[S, S] K_s = gp_exp_quad_cov(F_s, alpha, rho);
  matrix[S, S] L_s = cholesky_decompose(K_s);
  vector[S] beta_s = L_s * eta_s;

  matrix[S, S] K_d = phi * K_s;
  matrix[S, S] L_d = cholesky_decompose(K_d);

  matrix[S,D] beta_d;
  beta_d[:,D] = L_d * eta_d[:,D];
  for (d in 1:(D-1)) {
    beta_d[:,D-d] = L_d * eta_d[:,D-d] + beta_d[:,D-d+1];
  }

  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s[sid[n]] + beta_d[sid[n], did[n]];
  }
}

model {
  for (s in 1:S) {
    target += normal_lpdf(beta_s[s] + beta_d[s,180] | e_day_mu[s], e_day_sigma[s]);
  }
  target += std_normal_lpdf(to_vector(eta_d));
  target += normal_lpdf(phi | 0, phi_sigma) - normal_lccdf(0 | 0, phi_sigma);

  target += std_normal_lpdf(eta_s);
  target += gamma_lpdf(rho | rho_alpha, rho_beta);
  target += normal_lpdf(alpha | 0, alpha_sigma) - normal_lccdf(0 | 0, alpha_sigma);

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  matrix[S,D] theta;
  for (d in 1:D) {
    theta[:,d] = inv_logit(beta_s + beta_d[:,d]);
  }
  // Posterior predictions (poll results)
  // vector[N] p_rep = inv_logit(mu);
  // vector[N] y_rep = to_vector(binomial_rng(K, p_rep)) ./ to_vector(K);
  // Prior check
  // vector[D] ppc = inv_logit(beta_s[1] + to_vector(beta_sd[1,:]));
}
