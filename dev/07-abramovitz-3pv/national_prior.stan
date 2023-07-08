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
  int<lower=0> N;                   // number of rows in the training dataset
  int<lower=2> K;                   // number of possible choices
  matrix<lower=0, upper=1>[N,K] R;  // results of each election
  vector[N] inc_running;            // whether (1) or not (0) the incumbent is running
  vector[N] inc_approval;           // incumbent approval
  vector[N] third_party;            // presence (1) or absence (0) of a third party
  vector[N] real_gdp_growth;        // 2nd quarter gdp growth
  real biden_approval;              // incumbent approval for generated quantities
}
parameters {
  matrix[N_inc_status, K-1] inc_status_raw;
  matrix[N_inc_status, K-1] inc_status_approval_raw;
  vector[K-1] beta_inc_approval_raw;
  vector[K-1] beta_third_party_raw;
}
transformed parameters {
  matrix[N_inc_status, K] inc_status;
  matrix[N_inc_status, K] inc_status_approval;
  vector[K] beta_inc_approval;
  vector[K] beta_third_party;
  matrix[N,K] prob;

  // sum-to-zero parameterizations of each predictor
  inc_status = sum_to_zero(inc_status_raw);
  inc_status_approval = sum_to_zero(inc_status_approval_raw);
  beta_inc_approval = sum_to_zero(beta_inc_approval_raw);
  beta_third_party = sum_to_zero(beta_third_party_raw);

  // apply linear model
  for (i in 1:N) {
    row_vector[K] beta_approval_agg; // aggregate approval
    beta_approval_agg = to_row_vector(beta_inc_approval) + inc_status_approval[iid[i],];

    // apply linear model & convert to probability
    prob[i,] = inc_status[iid[i],] + beta_approval_agg*inc_approval[i] + to_row_vector(beta_third_party)*third_party[i];
    prob[i,] = to_row_vector(softmax(prob[i,]'));
  }

}
model {
  // gotta get *real* specific with priors here
  // based on iid:
  // 1: incumbent dem running
  // 2: incumbent dem party
  // 3: incumbent rep running
  // 4: incumbent rep party

  // global values repurposed for prior setting
  real sigma = 0.15;
  real even = 0.475;
  real inc_bonus = 0.025;
  real wt = 2;

  // incumbency status
  // incumbent running gets a boost; incumbent party doesn't (handled by interaction)
  inc_status_raw[1,1] ~ normal((even + inc_bonus)*wt, sigma);
  inc_status_raw[1,2] ~ normal((even - inc_bonus)*wt, sigma);
  inc_status_raw[2,1] ~ normal(wt*0.5, sigma);
  inc_status_raw[2,2] ~ normal(wt*0.5, sigma);
  inc_status_raw[3,1] ~ normal((even - inc_bonus)*wt, sigma);
  inc_status_raw[3,2] ~ normal((even + inc_bonus)*wt, sigma);
  inc_status_raw[4,1] ~ normal(wt*0.5, sigma);
  inc_status_raw[4,2] ~ normal(wt*0.5, sigma);

  // third party
  // both d/r lose ground evenly in the presence of a third party
  beta_third_party_raw ~ normal(-0.35, sigma);

  // approval
  // assumption is that as approval increases, both d/r will increase
  // (i.e., less approval = more 3rd party voters)
  beta_inc_approval_raw ~ normal(0.125, sigma);

  // interaction between incumbency status/net approval
  // an incumbent party gets a bigger boost when their incumbent is running
  inc_status_approval_raw[1,1] ~ normal(0.2, sigma);
  inc_status_approval_raw[1,2] ~ normal(-0.2, sigma);
  inc_status_approval_raw[2,1] ~ normal(0.075, sigma);
  inc_status_approval_raw[2,2] ~ normal(-0.075, sigma);
  inc_status_approval_raw[3,1] ~ normal(-0.2, sigma);
  inc_status_approval_raw[3,2] ~ normal(0.2, sigma);
  inc_status_approval_raw[4,1] ~ normal(-0.075, sigma);
  inc_status_approval_raw[4,2] ~ normal(0.075, sigma);

  for (i in 1:N) {
    R[i,] ~ dirichlet(prob[i,]');
  }
}
generated quantities {
  row_vector[K] prob_gq;

  {
    row_vector[K] beta_approval_agg;
    beta_approval_agg = to_row_vector(beta_inc_approval) + inc_status_approval[1,];
    prob_gq = inc_status[1,] + beta_approval_agg*biden_approval;
    prob_gq = to_row_vector(softmax(prob_gq'));
  }

}
