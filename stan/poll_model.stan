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
  int<lower=1> P;                      // Number of pollsters
  int<lower=1> G;                      // Number of groups (populations)
  int<lower=1> M;                      // Number of poll modes
  int<lower=1> C;                      // Number of candidate sponsors
  int<lower=1> S;                      // Number of states
  int<lower=1> D;                      // Number of days

  // Mapping IDs
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll
  array[N] int<lower=1, upper=G> gid;  // Map of group (population) to poll
  array[N] int<lower=1, upper=M> mid;  // Map of poll mode to poll
  array[N] int<lower=1, upper=C> cid;  // Map of candidate sponsor to poll
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll
  array[N] int<lower=1, upper=D> did;  // Map of day to poll

  // State feature matrix
  matrix[S, S] F_s;                    // State distance matrix in feature space

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // Fixed effect priors
  real<lower=0> beta_g_sigma;          // Scale for the group (population) bias
  real<lower=0> beta_m_sigma;          // Scale for the poll mode bias
  real<lower=0> beta_c_sigma;          // Scale for the candidate sponsor bias

  // Random effect priors
  real<lower=0> sigma_n_sigma;         // Scale for half-normal prior for raw poll bias
  real<lower=0> sigma_p_sigma;         // Scale for half-normal prior for pollster bias

  // State Gaussian Process priors
  real<lower=0> rho_alpha;             // Shape for gamma prior for state covariance length scale
  real<lower=0> rho_beta;              // Rate for gamma prior for state covariance length scale
  real<lower=0> alpha_sigma;           // Shape for half-normal prior for state covariance amplitude

  // Random walk priors
  real<lower=0> phi_sigma;             // Shape for half-normal prior for state covariance amplitude scaling

  // State election day priors
  vector[S] e_day_mu;                  // Logit-scale mean prior for election day
  vector<lower=0>[S] e_day_sigma;      // Shape for election day logit scale prior

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  // array[D] real D_arr = to_array_1d(linspaced_vector(D, 1, D)/D);
}

parameters {
  // Fixed effects
  vector[G] beta_g;                    // Group (population) bias
  vector[M] beta_m;                    // Poll mode bias
  vector[C] beta_c;                    // Candidate sponsor bias

  // Random effects
  vector[N] eta_n;                     // Raw poll bias
  vector[P] eta_p;                     // Pollster bias
  real<lower=0> sigma_n;               // Scale for raw poll bias
  real<lower=0> sigma_p;               // Scale for pollster bias

  // State Gaussian Process
  vector[S] eta_s;                     // State voting intent
  real<lower=0> rho;                   // Length scale for state covariance
  real<lower=0> alpha;                 // Amplitude for state covariance

  // Random walk
  matrix[S, D] eta_sd;                 // State by day voting entent
  real<lower=0> phi;                   // Scale for state covariance over random walk
}

transformed parameters{
  // Extract random parameters
  vector[N] beta_n = eta_n * sigma_n;
  vector[P] beta_p = eta_p * sigma_p;

  // Construct covariance matrix from feature space
  matrix[S, S] K_s = gp_exp_quad_cov(F_s, alpha, rho);
  matrix[S, S] L_s = cholesky_decompose(K_s);
  vector[S] beta_s = L_s * eta_s;

  // Construct random walk parameters
  matrix[S, S] K_d = phi * K_s;
  matrix[S, S] L_d = cholesky_decompose(K_d);
  matrix[S, D] beta_sd = L_d * eta_sd;
  for (d in 1:(D-1)) {
    for (s in 1:S) {
      beta_sd[s,d] = sum(beta_sd[s, d:(D-1)]);
    }
  }

  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s[sid[n]]
          + beta_sd[sid[n], did[n]]
          + beta_p[pid[n]]
          + beta_g[gid[n]]
          + beta_m[mid[n]]
          + beta_c[cid[n]]
          + beta_n[n];
  }
}

model {
  // Priors over fixed effects
  target += normal_lpdf(beta_g | 0, beta_g_sigma);
  target += normal_lpdf(beta_m | 0, beta_m_sigma);
  target += normal_lpdf(beta_c | 0, beta_c_sigma);

  // Priors over random effects
  target += std_normal_lpdf(eta_n);
  target += std_normal_lpdf(eta_p);
  target += normal_lpdf(sigma_n | 0, sigma_n_sigma) - normal_lccdf(0 | 0, sigma_n_sigma);
  target += normal_lpdf(sigma_p | 0, sigma_p_sigma) - normal_lccdf(0 | 0, sigma_p_sigma);

  // Priors over state Gaussian Process
  target += std_normal_lpdf(eta_s);
  target += gamma_lpdf(rho | rho_alpha, rho_beta);
  target += normal_lpdf(alpha | 0, alpha_sigma) - normal_lccdf(0 | 0, alpha_sigma);

  // Priors over random walk
  target += std_normal_lpdf(to_vector(eta_sd));
  target += normal_lpdf(phi | 0, phi_sigma) - normal_lccdf(0 | 0, phi_sigma);

  // Election day priors
  for (s in 1:S) {
    target += normal_lpdf(beta_s[s] + beta_sd[s,180] | e_day_mu[s], e_day_sigma[s]);
  }

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  array[S] vector[D] theta;
  for (s in 1:S) {
    theta[s,1:D] = inv_logit(beta_s[s] + to_vector(beta_sd[s,1:D]));
  }
  // Posterior predictions (poll results)
  // vector[N] p_rep = inv_logit(mu);
  // vector[N] y_rep = to_vector(binomial_rng(K, p_rep)) ./ to_vector(K);
  // Prior check
  // vector[D] ppc = inv_logit(beta_s[1] + to_vector(beta_sd[1,:]));
}
