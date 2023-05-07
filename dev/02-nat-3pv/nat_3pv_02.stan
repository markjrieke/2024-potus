data {
  int<lower=0> N; // number of rows in the training dataset
  array[N,3] int R; // results of each election
  real prior_dr; // prior for d/r alpha - on log scale
  real prior_other; // prior for other alpha - on log scale
}
parameters {
  vector[2] alpha;
}
transformed parameters {
  simplex[3] theta;
  theta = softmax(append_row(alpha, prior_other));
}
model {
  alpha ~ normal(prior_dr, 0.1);

  for (i in 1:N) {
    R[i,] ~ multinomial(theta);
  }
}

