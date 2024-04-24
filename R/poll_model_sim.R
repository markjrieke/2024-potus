# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

# set parameters ---------------------------------------------------------------

# 4 states in this fake shindig
population <- c(5, 20, 15, 10)

# state features
wwc_pct <- c(0.6, 0.5, 0.4, 0.5)
college <- c(0.2, 0.3, 0.4, 0.1)
income <- c(65000, 75000, 75000, 50000)

# convert to a state feature matrix
features <-
  matrix(
    c(wwc_pct, college, income),
    nrow = length(wwc_pct)
  )

# standardize across dimensions
for (f in 1:ncol(features)) {
  features[,f] <- standardize(features[,f])
}

# compute a distance matrix from features
distances <- matrix(0, nrow = nrow(features), ncol = nrow(features))
for (r in 1:nrow(distances)) {
  for (c in 1:ncol(distances)) {
    distances[r,c] <- (features[r,] - features[c,])^2 |> sum() |> sqrt()
  }
}

# covariance matrix along standardized distance vector
max_distances <- max(distances)
distances <- distances/max(distances)
Sigma <- 0.125 * exp(-(distances^2)/(2 * (0.5/max_distances)^2))
L <- cholesky_decompose(Sigma)

# state parameters
set.seed(2024)
eta <- rnorm(4, 0, 1)
beta_s <- (L %*% eta)[,1]

# national env is a weighted average of state params
beta_s <- c(beta_s, sum(beta_s * population)/sum(population))

# gaussian process offset
# election day = day == 180
Sigma <- cov_exp_quad(1:180/180, 0.02, 90/180)
L <- cholesky_decompose(Sigma)

# state by day parameters
set.seed(2020)
eta <- matrix(rnorm(180*4, 0, 1), nrow = 180, ncol = 4)
beta_sd <- t(L %*% eta)

# national env is a weighted average of state params
beta_sd <- rbind(beta_sd, colSums(beta_sd * population)/sum(population))

# pollster bias
set.seed(2016)
n_pollsters <- 8
beta_p <- rnorm(n_pollsters, 0, 0.01)

# group (population) bias
set.seed(2012)
n_groups <- 2
beta_g <- rnorm(n_groups, 0, 0.01)

# mode bias
beta_m <- c(0, 0.01)

# simulate data ----------------------------------------------------------------

n_polls <- 200

# probability of polling a specific state (based on close-ness)
state_probs <- (abs(expit(t(beta_s + beta_sd)[180,]) - 0.5)^-1)[1:length(population)]

# national polls correspond to index 5
state_probs <- c(state_probs, mean(state_probs))

# probability of poll occurring on a specific day
day_probs <- dnorm(1:179, 180, 60)

# average sample size (based on mode)
lambda <- c(1300, 2600)

# simulate polls!
set.seed(2008)
polls <-
  tibble(poll = 1:n_polls) %>%

  # poll characteristics
  bind_cols(state = sample(1:(length(population) + 1),
                           size = nrow(.),
                           replace = TRUE,
                           prob = state_probs)) %>%
  bind_cols(day = sample(1:179,
                         size = nrow(.),
                         replace = TRUE,
                         prob = day_probs)) %>%
  bind_cols(pollster = sample(1:length(beta_p),
                              size = nrow(.),
                              replace = TRUE,
                              prob = 1:length(beta_p))) %>%
  bind_cols(group = sample(1:n_groups,
                           size = nrow(.),
                           replace = TRUE)) %>%
  bind_cols(mode = sample(1:2,
                          size = nrow(.),
                          replace = TRUE)) %>%
  bind_cols(K = rpois(nrow(.), lambda[.$mode])) %>%

  # apply linear model
  mutate(beta_s = beta_s[state],
         beta_sd = pmap_dbl(list(state, day), ~beta_sd[..1, ..2]),
         beta_p = beta_p[pollster],
         beta_g = beta_g[group],
         beta_m = beta_m[mode]) %>%
  bind_cols(beta_n = rnorm(nrow(.), 0, 0)) %>%
  mutate(mu = beta_s + beta_sd + beta_p + beta_g + beta_m + beta_n,
         theta = expit(mu)) %>%

  # sample responses
  bind_cols(Y = rbinom(nrow(.), .$K, .$theta)) %>%
  select(state,
         day,
         K,
         Y,
         pid = pollster,
         gid = group,
         mid = mode)

# prep for modeling ------------------------------------------------------------

features2 <-
  rbind(features, population %*% features / sum(population))

distances2 <- matrix(0, nrow = nrow(features2), ncol = nrow(features2))
for (r in 1:nrow(distances2)) {
  for (c in 1:ncol(distances2)) {
    distances2[r,c] <- (features2[r,] - features2[c,])^2 |> sum() |> sqrt()
  }
}

polls %>%
  distinct(day) %>%
  arrange(day) %>%
  rowid_to_column("did") %>%
  right_join(polls, .)

stan_data <-
  list(
    N = nrow(polls),
    S = max(polls$state),
    S_nc = 4,
    D = 180,
    F_mat = distances,
    Nat_id = 5,
    Nat_wt = population/sum(population),
    sid = polls$state,
    did = polls$day,
    K = polls$K,
    Y = polls$Y
  )

poll_model <-
  cmdstan_model("stan/poll_model.stan")

poll_fit <-
  poll_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 250,
    iter_sampling = 250,
    chains = 4,
    parallel_chains = 4,
    init = 0.01,
    step_size = 0.002
  )

# blegh ------------------------------------------------------------------------

test <-
  polls %>%
  filter(state == 4)

stan_data <-
  list(
    N = nrow(test),
    D = 180,
    did = test$day,
    K = test$K,
    Y = test$Y
  )

test_model <-
  cmdstan_model("stan/test_model.stan")

test_fit <-
  test_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 500,
    iter_sampling = 500,
    chains = 4,
    parallel_chains = 4
  )

tmp <- test_fit$summary("theta_pred")

tmp %>%
  mutate(day = parse_number(variable)) %>%
  ggplot(aes(x = day,
             y = median)) +
  geom_ribbon(aes(ymin = q5,
                  ymax = q95),
              alpha = 0.25) +
  geom_line() +
  geom_point(data = test,
             mapping = aes(x = day,
                           y = Y/K,
                           size = K),
             shape = 21) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  expand_limits(y = c(0.4, 0.6))

tmp <- poll_fit$summary("theta")
polls %>%
  bind_cols(tmp %>% select(median, q5, q95)) %>%
  ggplot(aes(x = day)) +
  geom_ribbon(aes(ymin = q5,
                  ymax = q95),
              alpha = 0.25) +
  geom_point(aes(y = Y/K,
                 size = K),
             shape = 21) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  facet_wrap(~state) +
  theme_rieke()

polls %>%
  mutate(p = Y/K) %>%
  beta_interval(Y, K - Y) %>%
  ggplot(aes(x = day,
             y = p,
             size = K)) +
  geom_point(alpha = 0.25,
             shape = 21) +
  geom_smooth(se = FALSE) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  facet_wrap(~state) +
  theme_rieke()

