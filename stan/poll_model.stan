data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> S;                      // Number of states (including composites)
  int<lower=1> S_nc;                   // Number of states (excluding composites)

  // Data for creating composite states
  int<lower=1, upper=S> Nat_id;        // State ID for the national composite
  vector<lower=0>[S_nc] Nat_wt;        // Weight of each raw parameter in the national composite

  // Mapping IDs
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll
}

transformed data {
  int S_c = S - S_nc;                  // Number of composite states
}

parameters {
  vector[S_nc] beta_s_raw;
}

transformed parameters{
  // Append state parameters with composite states
  vector[S] beta_s = append_row(beta_s_raw, zeros_vector(S_c));
  beta_s[Nat_id] = dot_product(beta_s_raw, Nat_wt);

  // Estimate the linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_s[sid[n]];
  }
}

model {
  // priors
  target += normal_lpdf(mu | 0, 1);

  // likelihood
  target += binomial_logit_lpmf(Y | K, mu);
}

generated quantities {
  vector[N] theta = inv_logit(mu);
}
