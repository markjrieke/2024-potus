data {
  int<lower=0> N; // number of rows in the training dataset
  int<lower=2> K; // number of possible choices
  int<lower=1> D; // number of predictors (including intercept parameter)
  array[N,3] int R; // results of each election
  matrix[N,D] x; // model matrix
}
parameters {
  matrix[D, K-1] beta_raw;
}
transformed parameters {
  matrix[D, K] beta;
  matrix[N,K] x_beta;

  // sum-to-zero parameterization of each predictor (D in total)
  for (d in 1:D) {
    beta[d,] = append_col(beta_raw[d,], -sum(beta_raw[d,]));
  }

  x_beta = x * beta;
}
model {
  to_vector(beta) ~ normal(0, 5);

  for (i in 1:N) {
    R[i,] ~ multinomial(softmax(x_beta[i,]'));
  }
}
generated quantities {
  matrix[N,K] prob;
  for (i in 1:N) {
    prob[i,] = to_row_vector(softmax(x_beta[i,]'));
  }
}
