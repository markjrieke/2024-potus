data {
  int<lower=0> N;         // Number of observations
  int<lower=1> T;         // Number of time points

  array[N] int tid;       // Map time point to observations

  vector[N] Y;            // Response data

  real m0;                // State mean prior
  real<lower=0> C0;       // State sd prior

  // real<lower=0> W_sigma;  // State sd prior
  // real<lower=0> V_sigma;  // Observation sd prior

  real<lower=0> W_shape;
  real<lower=0> W_scale;
  real<lower=0> V_shape;
  real<lower=0> V_scale;
}

parameters {
  // vector[T] x;            // State
  real x0;
  vector[T-1] eta_t;      // State
  real<lower=0> W;        // State noise
  // real<lower=0> nu_w;     // State df
  real<lower=0.005> V;        // Observation noise
  // real<lower=0> nu_v;     // Observation df
}

transformed parameters {
  vector[T] x;
  x[1] = x0;
  x[2:T] = eta_t * W;
  for (t in 2:T) {
    x[t] += x[t-1];
  }
}

model {
  target += gamma_lpdf(W | W_shape, W_scale);
  target += gamma_lpdf(V | V_shape, V_scale);
  // target += normal_lpdf(W | 0, W_sigma) - normal_lccdf(0 | 0, W_sigma);
  // target += normal_lpdf(V | 0, V_sigma) - normal_lccdf(0 | 0, V_sigma);
  target += normal_lpdf(x0 | m0, C0);
  target += std_normal_lpdf(eta_t);

  // for (t in 2:T) {
  //   target += normal_lpdf(x[t] | x[t-1], W);
  // }

  for (n in 1:N) {
    target += normal_lpdf(Y[n] | x[tid[n]], V);
  }
}
