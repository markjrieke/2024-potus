data {
  int<lower=0> N; // number of rows in the training dataset
  matrix<lower=0, upper=1>[N,3] R; // results of each election
}
parameters {
  vector[2] alpha;
}
transformed parameters {
  simplex[3] theta;
  theta = softmax(append_row(alpha, 1.0));
}
model {
  alpha ~ normal(0, 1);

  for (i in 1:N) {
    R[i,]' ~ dirichlet(theta);
  }
}

