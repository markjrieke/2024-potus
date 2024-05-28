functions {
  #include /functions.stan
}

data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> D;                      // Number of days
  int<lower=1> R;                      // Number of raw states
  int<lower=0> A;                      // Number of aggregate states
  int<lower=1> S;                      // Number of total states
  int<lower=1> G;                      // Number of groups (populations)
  int<lower=1> M;                      // Number of poll modes
  int<lower=1> C;                      // Number of candidate sponsors
  int<lower=1> P;                      // Number of pollsters

  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll
  array[N] int<lower=1, upper=G> gid;  // Map of group (population) to poll
  array[N] int<lower=1, upper=M> mid;  // Map of poll mode to poll
  array[N] int<lower=1, upper=C> cid;  // Map of candidate sponsor to poll
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll

  // Reference Categories
  int<lower=1, upper=G> g_ref;         // ID for group (population) reference, lv
  int<lower=1, upper=C> c_ref;         // ID for candidate sponsor reference, None

  // Raw state data
  matrix[R, R] F_r;                    // Raw state distance matrix in feature space
  array[A] vector[R] wt;               // Raw weights per aggregate state

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // Fixed effect priors
  real<lower=0> beta_g_sigma;          // Scale for the group (population) bias
  real<lower=0> beta_c_sigma;          // Scale for the candidate sponsor bias

  // Random effect priors
  real<lower=0> sigma_n_sigma;         // Scale for half-normal prior for raw poll bias
  real<lower=0> sigma_p_sigma;         // Scale for half-normal prior for pollster bias
  real<lower=0> sigma_m_sigma;         // Scale for half-normal prior for mode bias

  // State Gaussian Process priors
  real<lower=0> rho_alpha;             // Shape for gamma prior for state covariance length scale
  real<lower=0> rho_beta;              // Rate for gamma prior for state covariance length scale

  // State polling bias priors
  real<lower=0> psi_sigma;             // Shape for half-normal prior for state covariance amplitude scaling

  // Random walk priors
  real<lower=0> phi_sigma;             // Shape for half-normal prior for state covariance amplitude scaling

  // State election day priors
  vector[R] alpha_mu_r;                // Raw logit-scale mean prior for election day
  vector[R] alpha_sigma_r;             // Raw logit-scale scale prior for election day

  // Generated quantities
  real<lower=0> omega;                 // Scale of beta rng
  vector[S] electors;                  // Number of electors per state

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  // Construct aggregate priors
  vector[S] alpha_mu = construct_aggregate(alpha_mu_r, wt);
}

parameters {
  // Fixed effects (excluding reference)
  vector[G-1] eta_g;                   // Group (population) bias
  vector[C-1] eta_c;                   // Candidate sponsor bias

  // Random effects
  vector[N] eta_n;                     // Raw poll bias
  vector[P] eta_p;                     // Pollster bias
  vector[M] eta_m;                     // Mode bias
  real<lower=0> sigma_n;               // Scale for raw poll bias
  real<lower=0> sigma_p;               // Scale for pollster bias
  real<lower=0> sigma_m;               // Scale for mode bias

  // State Gaussian Process
  vector[R] eta_r;                     // Raw state voting intent
  real<lower=0> rho;                   // Length scale for state covariance
  vector<lower=0>[R] alpha;            // Amplitude for state covariance

  // State Polling Bias
  vector[R] eta_rb;                    // Raw state polling bias
  real<lower=0> psi;                   // Scale for state covariance over polling bias

  // Random walk
  matrix[R, D] eta_rd;                 // Raw state by day voting entent
  real<lower=0> phi;                   // Scale for state covariance over random walk
}

transformed parameters{
  // Fixed effects with reference at 0
  vector[G] beta_g = add_reference(eta_g, g_ref);
  vector[C] beta_c = add_reference(eta_c, c_ref);

  // Extract random parameters
  vector[N] beta_n = eta_n * sigma_n;
  vector[P] beta_p = eta_p * sigma_p;
  vector[M] beta_m = eta_m * sigma_m;

  // Aggregate parameters derived from state covariance matrix
  vector[S] beta_s;
  vector[S] beta_b;
  matrix[S, D] beta_sd;

  // Construct parameters derived from state covariance matrix
  {
    // State parameters
    matrix[R, R] K_r = gp_exp_quad_cov(F_r, alpha, rho);
    matrix[R, R] L_r = cholesky_decompose(K_r);
    vector[R] beta_r = L_r * eta_r;

    // State-level polling bias
    matrix[R, R] L_rb = sqrt(psi) * L_r;
    vector[R] beta_rb = L_rb * eta_rb;

    // Random walk parameters
    matrix[R, R] L_d = sqrt(phi) * L_r;
    matrix[R, D] beta_rd = L_d * eta_rd;
    for (d in 1:(D-1)) {
      beta_rd[:,D-d] += beta_rd[:,D-d+1];
    }

    // Construct aggregate parameters
    beta_s = construct_aggregate(beta_r, wt);
    beta_b = construct_aggregate(beta_rb, wt);
    beta_sd = construct_aggregate(beta_rd, wt);
  }

  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = alpha_mu[sid[n]] + beta_s[sid[n]] + beta_sd[sid[n], did[n]]
          + beta_b[sid[n]]
          + beta_g[gid[n]]
          + beta_m[mid[n]]
          + beta_c[cid[n]]
          + beta_p[pid[n]]
          + beta_n[n];
  }
}

model {
  // Priors over fixed effects
  target += normal_lpdf(eta_g | 0, beta_g_sigma);
  target += normal_lpdf(eta_c | 0, beta_c_sigma);

  // Priors over random effects
  target += std_normal_lpdf(eta_n);
  target += std_normal_lpdf(eta_p);
  target += std_normal_lpdf(eta_m);
  target += normal_lpdf(sigma_n | 0, sigma_n_sigma) - normal_lccdf(0 | 0, sigma_n_sigma);
  target += normal_lpdf(sigma_p | 0, sigma_p_sigma) - normal_lccdf(0 | 0, sigma_p_sigma);
  target += normal_lpdf(sigma_m | 0, sigma_m_sigma) - normal_lccdf(0 | 0, sigma_m_sigma);

  // Priors over state Gaussian Process
  target += std_normal_lpdf(eta_r);
  target += gamma_lpdf(rho | rho_alpha, rho_beta);
  target += normal_lpdf(alpha | 0, alpha_sigma_r) - normal_lccdf(0 | 0, alpha_sigma_r);

  // Priors over state polling bias
  target += std_normal_lpdf(eta_rb);
  target += normal_lpdf(psi | 0, psi_sigma) - normal_lccdf(0 | 0, psi_sigma);

  // Priors over random walk
  target += std_normal_lpdf(to_vector(eta_rd));
  target += normal_lpdf(phi | 0, phi_sigma) - normal_lccdf(0 | 0, phi_sigma);

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  // Predicted voteshare in each state
  vector[S] mu_hat = inv_logit(alpha_mu + beta_s + beta_sd[:,D]);
  array[S] real theta = beta_rng(mu_hat * omega, (1 - mu_hat) * omega);

  // Predicted win conditions (state and presidential)
  vector[S] win_state = to_vector(round(theta));
  real<lower=0, upper=538> evs = dot_product(win_state, electors);
  real<lower=0, upper=1> win_pres = evs >= 270 ? 1.0 : 0.0;
  real<lower=0, upper=1> tie_pres = evs == 269 ? 1.0 : 0.0;
}
