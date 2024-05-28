/*
* Generate a Covariance Matrix with an Exponentiated Quadratic Kernel
*
* @param F: square matrix of pre-computed distances between observations
* @param sigma: amplitude of covariance matrix
* @param rho: length_scale of covariance matrix
*/
matrix gp_exp_quad_cov(matrix F,
                       vector sigma,
                       real rho) {
  int N = dims(F)[1];
  matrix [N,N] K;
  for (i in 1:(N - 1)) {
    K[i,i] = sigma[i]^2 + 1e-9;
    for (j in (i + 1):N) {
      K[i,j] = sigma[i] * sigma[j] * exp(-rho * square(F[i,j]));
      K[j,i] = K[i,j];
    }
  }
  K[N,N] = sigma[N]^2 + 1e-9;
  return K;
}

/*
* Add a reference value of 0 to a parameter vector
*
* @param eta: N-1 length vector of "raw" parameters
* @param ref: integer containing the reference position in the new N length vector
*/
vector add_reference(vector eta,
                     int ref) {
  int N = num_elements(eta) + 1;
  vector[N] beta;
  int off = 0;
  for (n in 1:N) {
    if (n == ref) {
      beta[n] = 0;
      off += 1;
    } else {
      beta[n] = eta[n - off];
    }
  }
  return beta;
}

/*
* Append a vector of parameters with aggregate parameters
*
* @param x: N length vector of parameters
* @param wt: A x N matrix of weights for each aggregate parameter
*/
vector construct_aggregate(vector x,
                           array[] vector wt) {
  int A = num_elements(wt[:,1]);
  int R = num_elements(x);
  int S = R + A;
  vector[S] y;
  y[1:R] = x;
  for (a in 1:A) {
    y[R + a] = dot_product(x, wt[a]);
  }
  return y;
}

/*
* Append a matrix of parameters with rows of aggregate parameters
*
* @param x: N x D matrix of parameters
* @param wt: A x N matrix of weights for each aggregate parameter
*/
matrix construct_aggregate(matrix x,
                           array[] vector wt) {
  int D = num_elements(x[1,:]);
  int R = num_elements(x[:,1]);
  int A = num_elements(wt[:,1]);
  int S = R + A;
  matrix[S, D] y;
  y[1:R, :] = x;
  for (a in 1:A) {
    y[R + a, :] = to_row_vector(transpose(x) * wt[a]);
  }
  return y;
}
