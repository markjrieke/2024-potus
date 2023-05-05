data {
  int<lower=0> N; // number of rows in the training dataset
  matrix<lower=0, upper=1>[N,3] R; // results of each election
}
parameters {
  vector[2] alpha;
}
transformed parameters {
  simplex[3] theta;
  theta = softmax(append_row(alpha, log(10)));
}
model {
  alpha ~ normal(log(80), 0.1);

  for (i in 1:N) {
    R[i,]' ~ dirichlet(theta);
  }
}

