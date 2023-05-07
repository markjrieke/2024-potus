data {
  int<lower=0> N; // number of rows in the training dataset
  array[N,3] int R; // results of each election
  real prior_dr; // prior for d/r alpha - on log scale
  real prior_other; // prior for other alpha - on log scale
  array[N] int iid; // index for incumbent status
}
parameters {
  vector[2] alpha_raw;
  matrix[2,4] inc_status;
}
transformed parameters {
  vector[3] alpha;
  array[N] simplex[3] theta;

  alpha = append_row(alpha_raw, -sum(alpha_raw));

  // for (i in 1:N) {
  //   for (j in 1:2) {
  //     theta[i,j] = alpha[j] + inc_status[j,iid[i]];
  //   }
  //   theta[i,3] = alpha[3];
  //   theta[i] = softmax(theta[i]);
  // }

}
model {
  // alpha ~ normal(prior_dr, 0.1);
  // to_vector(inc_status) ~ normal(0, 0.1);

  for (i in 1:N) {
    R[i,] ~ multinomial(alpha + inc_status);
  }
}

