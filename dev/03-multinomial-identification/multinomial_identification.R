# re-creating this walkthrough as a multinomial
# https://eleafeit.com/posts/2021-05-23-parameterization-of-multinomial-logit-models-in-stan/

# also referencing the stan docs here/here
# https://mc-stan.org/docs/stan-users-guide/multi-logit.html
# https://mc-stan.org/docs/stan-users-guide/parameterizing-centered-vectors.html

# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# direct recreation ------------------------------------------------------------

generate_mnl_data <- function(N=1000, K=2, J=3, beta=c(1, -2), alpha=c(1,0,-1)){
  if(length(beta) != K) stop ("incorrect number of parameters")
  Y <- rep(NA, N)
  X <- list(NULL)
  for (i in 1:N) {
    X[[i]] <- matrix(rnorm(J*K), ncol=K)
    Y[i] <- sample(x=J, size=1, prob=exp(alpha+X[[i]]%*%beta))
  }
  list(N=N, J=J, K=K, Y=Y, X=X, beta=beta, alpha=alpha)
}
d0 <- generate_mnl_data()

# basic multinomial logit model w/o intercepts
m1 <-
"data {
  int<lower=2> J; // of alternatives/outcomes
  int<lower=1> N;  // of observations
  int<lower=1> K; // of covariates
  int<lower=0,upper=J> Y[N];
  matrix[J,K] X[N];
}

parameters {
  vector[K] beta;  // attribute effects
}

model {
  for (i in 1:N)
    Y[i] ~ categorical_logit(X[i]*beta);
}"

d1 <- generate_mnl_data(N=1000, beta=c(-1,1), alpha=c(0,0,0))
p1 <- rstan::stan(model_code=m1, data=d1, iter=1000, chains=2, seed=20030601)

bayesplot::mcmc_trace(rstan::As.mcmc.list(p1, pars = "beta"))
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(p1, pars = "beta"),
                             true = d1$beta)

# not gonna do the bad version with intercepts for sake of time
# but, in general, is a result of identification

# mnl with prior on intercepts
m3 <- "
data {
  int<lower=2> J; // of alternatives/outcomes
  int<lower=1> N;  // of observations
  int<lower=1> K; // of covariates
  int<lower=0,upper=J> Y[N];
  matrix[J,K] X[N];
}

parameters {
  vector[J] alpha;
  vector[K] beta;
}

model {
  alpha ~ normal(0,1); // prior contrains alpha
  for (i in 1:N)
    Y[i] ~ categorical_logit(alpha + X[i]*beta);
}"

d2 <- generate_mnl_data(N=1000, beta=c(1,-1), alpha=c(1, 0, -2))
p3 <- rstan::stan(model_code = m3, data = d2, iter = 1000, chains = 2, seed = 19730715)

bayesplot::mcmc_trace(rstan::As.mcmc.list(p3, pars = c("alpha", "beta")))
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(p3, pars = c("alpha", "beta")),
                             true = c(d2$alpha, d2$beta))

# mnl with fix-one-to-zero constraint

m4 <- "
data {
  int<lower=2> J; // of alternatives/outcomes
  int<lower=1> N;  // of observations
  int<lower=1> K; // of covariates
  int<lower=0,upper=J> Y[N];
  matrix[J,K] X[N];
}

parameters {
  vector[J-1] alpha_raw;
  vector[K] beta;
}

transformed parameters {
  vector[J] alpha;
  alpha = append_row(0, alpha_raw);
}

model {
  for (i in 1:N)
    Y[i] ~ categorical_logit(alpha + X[i]*beta);
}"

p4 <- rstan::stan(model_code = m4, data=d2, iter=1000, chains=2, seed=19730715)

bayesplot::mcmc_trace(rstan::As.mcmc.list(p4, pars = "alpha"))
shifted_alpha <- d2$alpha - d2$alpha[1]
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(p4, pars = "alpha"), true = shifted_alpha)

# skipping bad implementation of sum-to-zero constraint

# mnl with sum-to-zero constraint
m6 <- "
data {
  int<lower=2> J; // of alternatives/outcomes
  int<lower=1> N;  // of observations
  int<lower=1> K; // of covariates
  int<lower=0,upper=J> Y[N];
  matrix[J,K] X[N];
}

parameters {
  vector[J-1] alpha_raw; // unconstrained UPC intercepts
  vector[K] beta;
}

transformed parameters{
  vector[J] alpha;
  alpha = append_row(-sum(alpha_raw), alpha_raw); // sum to zero constraint
}

model {
  for (i in 1:N)
    Y[i] ~ categorical_logit(alpha + X[i]*beta);
}"

p6 <- rstan::stan(model_code = m6, data=d2, iter=1000, chains=2, seed=19730715)

shifted_alpha <- d2$alpha - (1/d2$J)*sum(d2$alpha)
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(p6, pars = "alpha"),
                             true = shifted_alpha)

# stan multi logit -------------------------------------------------------------

N <- 500
K <- 3
D <- 2

beta <- matrix(nrow = D, ncol = K)

beta[1,] <- c(1, 1, -1)
beta[2,] <- c(-2, 2, 0)

x <- matrix(nrow = N, ncol = D)

x[,1] <- 1
x[,2] <- sample(0:1, nrow(x), replace = TRUE)

x_beta <- x %*% beta

y <- vector(length = N)

for (i in 1:N) {
  y[i] <- sample(1:3, 1, prob = softmax(x_beta[i,]))
}

stan_data <-
  list(
    K = K,
    N = N,
    D = D,
    y = y,
    x = x
  )

mnl1_code <- "
data {
  int K;
  int N;
  int D;
  array[N] int y;
  matrix[N, D] x;
}
parameters {
  matrix[D, K] beta;
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}
"

mnl1 <-
  rstan::stan(
    model_code = mnl1_code,
    data = stan_data,
    chains = 1
  )

mnl1 %>%
  rethinking::precis(pars = "beta",
                     depth = 3)

mnl2_code <- "
data {
  int K;
  int N;
  int D;
  array[N] int y;
  matrix[N, D] x;
}
transformed data {
  vector[D] zeros = rep_vector(0, D);
}
parameters {
  matrix[D, K - 1] beta_raw;
}
transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros);
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}
"

mnl2 <-
  rstan::stan(
    model_code = mnl2_code,
    data = stan_data,
    chains = 1
  )

mnl2 %>%
  rethinking::precis(pars = "beta", depth = 3)

mnl3_code <- "
data {
  int K;
  int N;
  int D;
  array[N] int y;
  matrix[N, D] x;
}
transformed data {
  vector[D] zeros = rep_vector(0, D);
}
parameters {
  matrix[D, K - 1] beta_raw;
}
transformed parameters {
  matrix[D, K] beta;
  vector[D] beta_append;

  for (d in 1:D) {
    beta_append[d] = -sum(beta_raw[d,]);
  }

  beta = append_col(beta_raw, beta_append);
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}
"

mnl3 <-
  rstan::stan(
    model_code = mnl3_code,
    data = stan_data,
    chains = 1
  )

mnl3 %>%
  rethinking::precis(pars = "beta", depth = 3)

shifted_beta <- beta - (1/K)*sum(beta)

# dont @ me
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[1,1]"), true = shifted_beta[1,1])
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[1,2]"), true = shifted_beta[1,2])
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[1,3]"), true = shifted_beta[1,3])
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[2,1]"), true = shifted_beta[2,1])
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[2,2]"), true = shifted_beta[2,2])
bayesplot::mcmc_recover_hist(rstan::As.mcmc.list(mnl3, pars = "beta[2,3]"), true = shifted_beta[2,3])
