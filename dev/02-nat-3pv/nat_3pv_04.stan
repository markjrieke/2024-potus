data {
  int<lower=0> N; // number of rows in the training dataset
  int<lower=2> K; // number of possible choices
  int<lower=1> N_inc_party; // number of incumbent parties
  int<lower=1> N_inc_status; // number of incumbent statuses
  matrix<lower=0, upper=1>[N,K] R; // results of each election
  array[N] int iid; // incumbent status id
  array[N] int pid; // incumbent party id
}
parameters {
  matrix[N_inc_party, K-1] inc_party_raw;
  matrix[N_inc_status, K-1] inc_status_raw;
}
transformed parameters {
  matrix[N_inc_party, K] inc_party;
  matrix[N_inc_status, K] inc_status;
  matrix[N,K] prob;

  // sum-to-zero parameterizations of each predictor
  for (n in 1:N_inc_party) {
    inc_party[n,] = append_col(inc_party_raw[n,], -sum(inc_party_raw[n,]));
  }
  for (n in 1:N_inc_status) {
    inc_status[n,] = append_col(inc_status_raw[n,], -sum(inc_status_raw[n,]));
  }

  // apply linear model
  for (i in 1:N) {
    for (j in 1:K) {
      prob[i,j] = inc_party[pid[i],j] + inc_status[iid[i],j];
    }
    prob[i,] = to_row_vector(softmax(prob[i,]'));
  }

}
model {
  to_vector(inc_party_raw) ~ normal(0, 1);
  to_vector(inc_status_raw) ~ normal(0, 1);

  for (i in 1:N) {
    R[i,] ~ dirichlet(prob[i,]');
  }
}

