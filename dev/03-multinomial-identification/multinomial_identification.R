# re-creating this walkthrough as a multinomial
# https://eleafeit.com/posts/2021-05-23-parameterization-of-multinomial-logit-models-in-stan/

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
