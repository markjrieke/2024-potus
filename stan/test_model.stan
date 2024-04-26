data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> S;                      // Number of states
  int<lower=1> D;                      // Number of observed days (e-day == D)
  int<lower=1> P;                      // Number of pollsters
  int<lower=1> G;                      // Number of groups (populations)
  int<lower=1> M;                      // Number of poll modes

  // Mapping IDs
  // array[N] int<lower=1, upper=S> sid;  // Map of state to poll
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll
  array[N] int<lower=1, upper=G> gid;  // Map of group (population) to poll
  array[N] int<lower=1, upper=M> mid;  // Map of mode to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // Election day priors
  vector[S] e_day_mu;                  // Logit-scale election day prior mean
  vector<lower=0>[S] e_day_sigma;      // Logit-scale election day prior scale

  // Fixed effect priors
  real<lower=0> beta_s_sigma;
  real<lower=0> beta_g_sigma;          // Scale for group (population) bias
  real<lower=0> beta_m_sigma;          // Scale for mode bias

  // Random effect priors
  real<lower=0> sigma_p_sigma;         // Scale for half-normal prior for pollster bias
  real<lower=0> sigma_n_sigma;         // Scale for half-normal prior for raw poll bias

  // Gaussian Process priors (day)
  real<lower=0> rho_d_shape;           // Shape param for gamma prior over length-scale
  real<lower=0> rho_d_rate;            // Rate param for gamma prior over length-scale
  real<lower=0> sigma_d_sigma;         // Scale for half-normal prior over amplitude

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  array[D] real D_arr = to_array_1d(linspaced_vector(D, 1, D)/D);
}

parameters {
  // Fixed effects
  real beta_s;                         // State-specific parameter
  vector[G] beta_g;                    // Group (population) bias
  vector[M] beta_m;                    // Mode bias

  // Random effects
  vector[P] eta_p;                     // Pollster bias
  vector[N] eta_n;                     // Raw poll bias
  real<lower=0> sigma_p;               // Scale for pollster bias
  real<lower=0> sigma_n;               // Scale for raw poll bias

  // Gaussian process parameters
  real<lower=0> rho_d;                 // Length-scale parameter for day GP
  real<lower=0> sigma_d;               // Amplitude parameter for day GP
  vector[D] eta_d;
}

transformed parameters{
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

  // Extract random parameters
  vector[P] beta_p = eta_p * sigma_p;
  vector[N] beta_n = eta_n * sigma_n;

  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s + beta_d[did[n]] + beta_p[pid[n]] + beta_g[gid[n]] + beta_m[mid[n]] + beta_n[n];
  }
}

model {
  // Election day priors
  target += normal_lpdf(beta_s + beta_d[180] | e_day_mu, e_day_sigma);

  // Priors over fixed effects
  target += normal_lpdf(beta_s | 0, beta_s_sigma);
  target += normal_lpdf(beta_g | 0, beta_g_sigma);
  target += normal_lpdf(beta_m | 0, beta_m_sigma);

  // Priors over random effects
  target += std_normal_lpdf(eta_p);
  target += std_normal_lpdf(eta_n);
  target += normal_lpdf(sigma_p | 0, sigma_p_sigma) - normal_lccdf(0 | 0, sigma_p_sigma);
  target += normal_lpdf(sigma_n | 0, sigma_n_sigma) - normal_lccdf(0 | 0, sigma_n_sigma);

  // Priors over Gaussian Process (day)
  target += gamma_lpdf(rho_d | rho_d_shape, rho_d_rate);
  target += normal_lpdf(sigma_d | 0, sigma_d_sigma) - normal_lccdf(0 | 0, sigma_d_sigma);
  target += std_normal_lpdf(eta_d);

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  vector[D] theta_pred = inv_logit(beta_s + beta_d);
}
