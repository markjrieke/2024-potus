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
  int<lower=1> H;                      // Number of democratic nominees

  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll
  array[N] int<lower=1, upper=G> gid;  // Map of group (population) to poll
  array[N] int<lower=1, upper=M> mid;  // Map of poll mode to poll
  array[N] int<lower=1, upper=C> cid;  // Map of candidate sponsor to poll
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll
  array[N] int<lower=1, upper=H> hid;  // Map of democratic nominee to poll

  // Reference Categories
  int<lower=1, upper=G> g_ref;         // ID for group (population) reference, lv
  int<lower=1, upper=C> c_ref;         // ID for candidate sponsor reference, None
  int<lower=1, upper=H> h_ref;         // ID for democratic nominee

  // Hypothetical Candidate Corrections
  vector<lower=0, upper=1>[N] hyp;     // Takes value of (1) for Harris polls prior to announcement

  // Internal Poll Corrections
  vector<lower=-1, upper=1>[N] trn;    // Takes value of (1) for Harris internals and (-1) for Trump internals

  // Raw state data
  matrix[R, R] F_r;                    // Raw state distance matrix in feature space
  array[A] vector[R] wt;               // Raw weights per aggregate state

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // Fixed effect priors
  real<lower=0> beta_g_sigma;          // Scale for the group (population) bias
  real<lower=0> beta_c_sigma;          // Scale for the candidate sponsor bias
  real<lower=0> beta_hyp_sigma;        // Scale for the pre-announcement bias
  real<lower=0> beta_trn_sigma;        // Scale for the internal-poll bias

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
  array[H] vector[R] alpha_mu_r;       // Raw logit-scale mean prior for election day
  array[H] vector[R] alpha_sigma_r;    // Raw logit-scale scale prior for election day

  // Generated quantities
  real<lower=0> omega;                 // Scale of multivariate normal rng
  vector[S] electors;                  // Number of electors per state
  matrix[S, S] F_s;                    // State distance matrix in feature space

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  // Construct aggregate priors
  array[H] vector[S] alpha_mu;
  for (h in 1:H) {
    alpha_mu[h,:] = construct_aggregate(alpha_mu_r[h,:], wt);
  }
}

parameters {
  // Fixed effects (excluding reference)
  vector[G-1] eta_g;                   // Group (population) bias
  vector[C-1] eta_c;                   // Candidate sponsor bias
  real eta_hyp;                        // Hypothetical candidate correction
  real<lower=0> eta_trn;               // Internal poll correction

  // Random effects
  vector[N] eta_n;                     // Raw poll bias
  vector[P] eta_p;                     // Pollster bias
  vector[M] eta_m;                     // Mode bias
  real<lower=0> sigma_n;               // Scale for raw poll bias
  real<lower=0> sigma_p;               // Scale for pollster bias
  real<lower=0> sigma_m;               // Scale for mode bias

  // State Gaussian Process
  array[H] vector[R] eta_r;            // Raw state voting intent
  real<lower=0> rho;                   // Length scale for state covariance
  array[H] vector<lower=0>[R] alpha;   // Amplitude for state covariance

  // State Polling Bias
  vector[R] eta_rb;                    // Raw state polling bias
  real<lower=0> psi;                   // Scale for state covariance over polling bias

  // Random walk
  array[H] matrix[R, D] eta_rd;        // Raw state by day voting entent
  real<lower=0> phi;                   // Scale for state covariance over random walk
}

transformed parameters{
  // Fixed effects with reference at 0
  vector[G] beta_g = add_reference(eta_g, g_ref);
  vector[C] beta_c = add_reference(eta_c, c_ref);
  vector[N] beta_hyp = eta_hyp * hyp;
  vector[N] beta_trn = eta_trn * trn;

  // Extract random parameters
  vector[N] beta_n = eta_n * sigma_n;
  vector[P] beta_p = eta_p * sigma_p;
  vector[M] beta_m = eta_m * sigma_m;

  // Aggregate parameters derived from state covariance matrix
  array[H] vector[S] beta_s;
  array[H] vector[S] beta_b;
  array[H] matrix[S, D] beta_sd;

  // Construct parameters derived from state covariance matrix
  {
    // Candidate-specific transformations
    for (h in 1:H) {

      // State parameters
      matrix[R, R] K_r = gp_exp_quad_cov(F_r, alpha[h], rho);
      matrix[R, R] L_r = cholesky_decompose(K_r);
      vector[R] beta_r = L_r * eta_r[h];

      // State-level polling bias
      matrix[R, R] L_rb = sqrt(psi) * L_r;
      vector[R] beta_rb = L_rb * eta_rb;

      // Random walk parameters
      matrix[R, R] L_d = sqrt(phi) * L_r;
      matrix[R, D] beta_rd = L_d * eta_rd[h];
      for (d in 1:(D-1)) {
        beta_rd[:,D-d] += beta_rd[:,D-d+1];
      }

      // Construct aggregate parameters
      beta_s[h] = construct_aggregate(beta_r, wt);
      beta_b[h] = construct_aggregate(beta_rb, wt);
      beta_sd[h] = construct_aggregate(beta_rd, wt);
    }
  }

  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = alpha_mu[hid[n], sid[n]] + beta_s[hid[n], sid[n]] + beta_sd[hid[n], sid[n], did[n]]
          + beta_b[hid[n], sid[n]]
          + beta_g[gid[n]]
          + beta_m[mid[n]]
          + beta_c[cid[n]]
          + beta_p[pid[n]]
          + beta_hyp[n]
          + beta_trn[n]
          + beta_n[n];
  }
}

model {
  // Priors over fixed effects
  target += normal_lpdf(eta_g | 0, beta_g_sigma);
  target += normal_lpdf(eta_c | 0, beta_c_sigma);
  target += normal_lpdf(eta_hyp | 0, beta_hyp_sigma);
  target += normal_lpdf(eta_trn | 0, beta_trn_sigma);

  // Priors over random effects
  target += std_normal_lpdf(eta_n);
  target += std_normal_lpdf(eta_p);
  target += std_normal_lpdf(eta_m);
  target += normal_lpdf(sigma_n | 0, sigma_n_sigma) - normal_lccdf(0 | 0, sigma_n_sigma);
  target += normal_lpdf(sigma_p | 0, sigma_p_sigma) - normal_lccdf(0 | 0, sigma_p_sigma);
  target += normal_lpdf(sigma_m | 0, sigma_m_sigma) - normal_lccdf(0 | 0, sigma_m_sigma);

  // Priors over state Gaussian Process
  target += gamma_lpdf(rho | rho_alpha, rho_beta);

  // Priors over state polling bias
  target += std_normal_lpdf(eta_rb);
  target += normal_lpdf(psi | 0, psi_sigma) - normal_lccdf(0 | 0, psi_sigma);

  // Priors over random walk
  target += normal_lpdf(phi | 0, phi_sigma) - normal_lccdf(0 | 0, phi_sigma);

  // Candidate-specific priors
  for (h in 1:H) {
    target += std_normal_lpdf(eta_r[h]);
    target += std_normal_lpdf(to_vector(eta_rd[h]));
    target += normal_lpdf(alpha[h] | 0, alpha_sigma_r[h]) - normal_lccdf(0 | 0, alpha_sigma_r[h]);
  }

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  // Predicted voteshare in each state
  vector[S] mu_hat;
  vector[S] theta;
  {
    matrix[S, S] K_s = gp_exp_quad_cov(F_s, rep_vector(omega, S), rho);
    matrix[S, S] L_s = cholesky_decompose(K_s);
    mu_hat = alpha_mu[h_ref] + beta_s[h_ref] + beta_sd[h_ref,:,D];
    theta = inv_logit(multi_normal_cholesky_rng(mu_hat, L_s));
  }

  // Predicted win conditions (state and presidential)
  vector[S] win_state = to_vector(round(theta));
  real<lower=0, upper=538> evs = dot_product(win_state, electors);
  real<lower=0, upper=1> win_pres = evs >= 270 ? 1.0 : 0.0;
  real<lower=0, upper=1> tie_pres = evs == 269 ? 1.0 : 0.0;
}
