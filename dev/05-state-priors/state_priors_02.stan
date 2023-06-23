functions {
  // convert a N by K-1 matrix of parameters, phi, to a N by K matrix
  // where the Kth column of the matrix satisfies the sum-to-zero constraint
  matrix sum_to_zero(matrix phi) {
    matrix[rows(phi), cols(phi) + 1] phi_out;

    for (r in 1:rows(phi)) {
      phi_out[r,] = append_col(phi[r,], -sum(phi[r,]));
    }

    return(phi_out);
  }

  // convert a K-1 vector of parameters, phi, to a K-length vector
  // where the Kth element of the vector satisfies the sum-to-zero constraint
  vector sum_to_zero(vector phi) {
    vector[size(phi) + 1] phi_out;
    phi_out = append_row(phi, -sum(phi));

    return(phi_out);
  }
}
data {
  int<lower=0> N; // number of observations
  int<lower=2> K; // number of categories (d/r/o)
  matrix<lower=0, upper=1>[N,K] R; // d/r/o results in each state
  int<lower=1> N_inc_status; // number of possible incumbent statuses
  array[N] int iid; // indicator for incumbency status
  vector[N] pvi_3d; // state partisan lean (towards democrats)
  vector[N] pvi_3r; // state partisan lean (towards republicans)
}
parameters {
  matrix[N_inc_status, K-1] inc_status_raw;
  vector[K-1] beta_3d_raw;
  vector[K-1] beta_3r_raw;
}
transformed parameters {
  matrix[N_inc_status, K] inc_status;
  vector[K] beta_3d;
  vector[K] beta_3r;
  matrix[N, K] prob;

  // apply sum-to-zero constraint on parameters
  inc_status = sum_to_zero(inc_status_raw);
  beta_3d = sum_to_zero(beta_3d_raw);
  beta_3r = sum_to_zero(beta_3r_raw);

  // apply linear model
  for (i in 1:N) {
    prob[i,] = inc_status[iid[i],] +
               to_row_vector(beta_3d)*pvi_3d[i] +
               to_row_vector(beta_3r)*pvi_3r[i];
    prob[i,] = to_row_vector(softmax(prob[i,]'));
  }

}
model {
  // priors
  to_vector(inc_status_raw) ~ normal(0.475, 1);
  beta_3d ~ normal(0, 1);
  beta_3r ~ normal(0, 1);

  // fit model
  for (i in 1:N) {
    R[i,] ~ dirichlet(prob[i,]');
  }
}
