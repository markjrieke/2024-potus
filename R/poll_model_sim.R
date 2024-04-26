# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

# set parameters ---------------------------------------------------------------

# states in this fake shindig
n_states <- 8
set.seed(123)
population <- sample(1:20, n_states, replace = TRUE)

# state features
set.seed(456)
wwc_pct <- sample(20:60/100, n_states, replace = TRUE)
college <- sample(10:40/100, n_states, replace = TRUE)
income <- sample(50:100, n_states, replace = TRUE) * 1000

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
Sigma <- 1 * exp(-(distances^2)/(2 * (0.5/max_distances)^2))
L <- cholesky_decompose(Sigma)

# state parameters
set.seed(2024)
eta <- rnorm(n_states, 0, 1)
beta_s <- (L %*% eta)[,1]

# national env is a weighted average of state params
beta_s <- c(beta_s, sum(beta_s * population)/sum(population))

# gaussian process offset
# election day = day == 180
Sigma <- cov_exp_quad(1:180/180, 0.05, 90/180)
L <- cholesky_decompose(Sigma)

# state by day parameters
set.seed(2020)
eta <- matrix(rnorm(180*n_states, 0, 1), nrow = 180, ncol = n_states)
beta_sd <- t(L %*% eta)

# national env is a weighted average of state params
beta_sd <- rbind(beta_sd, colSums(beta_sd * population)/sum(population))

# pollster bias
set.seed(2016)
n_pollsters <- 8
beta_p <- rnorm(n_pollsters, 0, 0.1)

# group (population) bias
set.seed(2012)
n_groups <- 2
beta_g <- rnorm(n_groups, 0, 0.1)

# mode bias
beta_m <- c(0, 0.05)

# sponsor/candidate bias
beta_c <- c(0, 0.05, -0.05)

# simulate data ----------------------------------------------------------------

n_polls <- 360

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
    S = 1,
    D = 180,
    P = max(test$pid),
    G = max(test$gid),
    M = max(test$mid),
    did = test$day,
    pid = test$pid,
    gid = test$gid,
    mid = test$mid,
    K = test$K,
    Y = test$Y,
    e_day_mu = 0,
    e_day_sigma = 0.1,
    beta_s_sigma = 1,
    beta_g_sigma = 0.05,
    beta_m_sigma = 0.05,
    sigma_p_sigma = 0.05,
    sigma_n_sigma = 0.05,
    rho_d_shape = 3,
    rho_d_rate = 6,
    sigma_d_sigma = 0.05,
    prior_check = 0
  )

test_model <-
  cmdstan_model("stan/test_model.stan")

test_fit <-
  test_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    init = 0.01,
    step_size = 0.002,
    refresh = 100
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
  geom_line(data = (beta_s + beta_sd) %>%
              t() %>%
              as_tibble() %>%
              select(V4) %>%
              mutate(across(everything(), expit)) %>%
              rowid_to_column("day"),
            mapping = aes(x = day,
                          y = V4),
            color = "royalblue") +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  expand_limits(y = c(0.4, 0.6))

preds <- test_fit$draws("theta_pred", format = "df")
preds %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta")) %>%
  nest(data = -.draw) %>%
  slice_sample(n = 250) %>%
  unnest(data) %>%
  mutate(name = parse_number(name)) %>%
  ggplot(aes(x = name,
             y = value,
             group = .draw)) +
  geom_line(alpha = 0.125) +
  theme_rieke() +
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

