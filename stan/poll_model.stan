data {
  // Dimensions of the dataset
  int<lower=0> N;                      // Number of observations
  int<lower=1> S;                      // Number of states (including composites)
  // int<lower=1> S_nc;                   // Number of states (excluding composites)

  // Mapping IDs
  array[N] int<lower=1, upper=S> sid;  // Map of state to poll

  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll
}

parameters {
  vector[S_nc] beta_s_nc;
}

transformed parameters{
  vector[S] beta_s;
}

transformed parameters {
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
