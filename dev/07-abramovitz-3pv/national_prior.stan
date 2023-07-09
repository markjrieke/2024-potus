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
  array[N] int inc_status;          // whether (1) or not (2) the incumbent is running
  vector[N] inc_approval;           // incumbent approval
  vector[N] third_party;            // presence (1) or absence (0) of a third party
  vector[N] real_gdp_growth;        // 2nd quarter gdp growth
  real biden_approval;              // incumbent approval for generated quantities
  real biden_gdp;                   // 2nd quarter gdp for generated quantities
}
parameters {
  vector[K-1] alpha_raw;
  matrix[2, K-1] beta_inc_raw;
  vector[K-1] beta_inc_app_raw;
  vector[K-1] beta_gdp_raw;
  vector[K-1] beta_third_party_raw;
}
transformed parameters {
  vector[K] alpha;
  matrix[2, K] beta_inc;
  vector[K] beta_inc_app;
  vector[K] beta_gdp;
  vector[K] beta_third_party;
  matrix[N, K] prob;

  // sum-to-zero parameterizations of each predictor
  alpha = sum_to_zero(alpha_raw);
  beta_inc = sum_to_zero(beta_inc_raw);
  beta_inc_app = sum_to_zero(beta_inc_app_raw);
  beta_gdp = sum_to_zero(beta_gdp_raw);
  beta_third_party = sum_to_zero(beta_third_party_raw);

  for (i in 1:N) {
    // apply linear model
    prob[i,] = to_row_vector(alpha) +
               to_row_vector(beta_inc[inc_status[i],]) +
               to_row_vector(beta_inc_app)*inc_approval[i] +
               to_row_vector(beta_gdp)*real_gdp_growth[i] +
               to_row_vector(beta_third_party)*third_party[i];

    // convert to probabilities
    prob[i,] = to_row_vector(softmax(prob[i,]'));
  }

}
model {
  // global values repurposed for prior setting
  real sigma = 0.1;
  real even = 0.475;
  real wt = 2;

  // get real specific with priors
  alpha_raw ~ normal(even*wt, sigma);
  beta_inc_raw[1,1] ~ normal(0.1, sigma);
  beta_inc_raw[1,2] ~ normal(-0.1, sigma);
  beta_inc_raw[2,1] ~ normal(-0.05, sigma);
  beta_inc_raw[2,2] ~ normal(0.05, sigma);
  beta_inc_app_raw[1] ~ normal(0.05, sigma);
  beta_inc_app_raw[2] ~ normal(-0.05, sigma);
  beta_gdp_raw[1] ~ normal(0.05, sigma);
  beta_gdp_raw[2] ~ normal(-0.05, sigma);
  beta_third_party_raw ~ normal(-0.15, sigma);

  for (i in 1:N) {
    R[i,] ~ dirichlet(prob[i,]');
  }
}
generated quantities {
  row_vector[K] prob_gq;

  {
    prob_gq = to_row_vector(alpha) +
              to_row_vector(beta_inc[1,]) +
              to_row_vector(beta_inc_app)*biden_approval +
              to_row_vector(beta_gdp)*biden_gdp;

    prob_gq = to_row_vector(softmax(prob_gq'));
  }

}
