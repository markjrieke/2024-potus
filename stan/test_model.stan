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

  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll

  // blegh
  real e_day_mu;
  real e_day_sigma;
  real eta_sigma;

  // Debug
  int<lower=0, upper=1> prior_check;
}

transformed data {
  // array[D] real D_arr = to_array_1d(linspaced_vector(D, 1, D)/D);
}

parameters {
  vector[D] eta_d;
  real beta_s;
}

transformed parameters{
  vector[D] beta_d;
  beta_d[D] = eta_d[D];
  for (d in 1:(D-1)) {
    beta_d[D-d] = eta_d[D-d] + beta_d[D-d+1];
  }

  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s + beta_d[did[n]];
  }
}

model {
  target += normal_lpdf(beta_s + beta_d[180] | e_day_mu, e_day_sigma);
  target += normal_lpdf(eta_d | 0, eta_sigma);

  // likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  vector[D] theta;
  for (d in 1:D) {
    theta[d] = inv_logit(beta_s + beta_d[d]);
  }
  // Posterior predictions (poll results)
  // vector[N] p_rep = inv_logit(mu);
  // vector[N] y_rep = to_vector(binomial_rng(K, p_rep)) ./ to_vector(K);
  // Prior check
  // vector[D] ppc = inv_logit(beta_s[1] + to_vector(beta_sd[1,:]));
}
