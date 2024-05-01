# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

# set parameters ---------------------------------------------------------------

# states in this fake shindig
n_states <- 17
set.seed(1)
population <- sample(1:20, n_states, replace = TRUE)

# state prior mu
set.seed(2)
e_day_mu <- rnorm(n_states, 0, 0.5)

# state features
set.seed(3)
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
K_r <- 0.05 * exp(-(distances^2)/(2 * 0.125^2))
L_r <- cholesky_decompose(K_r)

# state parameters
set.seed(4)
eta_r <- rnorm(n_states, 0, 1)
beta_r <- (L_r %*% eta_r)[,1] + e_day_mu

# identify aggregate states
aggregates <-
  matrix(
    c(rep(1, 3), rep(0, n_states - 3),
      rep(0, 3), rep(1, 3), rep(0, n_states - 6),
      rep(1, n_states)),
    ncol = 3
  )

# create state weights
wt <- t(aggregates * population)
for (r in 1:nrow(wt)) {
  wt[r,] <- wt[r,] / sum(wt[r,])
}

# aggregate states are weighted avg of state params
beta_s <- beta_r
for (r in 1:nrow(wt)) {
  beta_s <- c(beta_s, sum(wt[r,] * beta_r))
}

# random walk covariance matrix
K_d <- K_r * 0.025
L_d <- cholesky_decompose(K_d)

# state by day parameters
# here [s,d] is the DAYS UNTIL E-DAY (rather than day)
set.seed(5)
eta_rd <- matrix(rnorm(180*n_states), nrow = n_states, ncol = 180)
beta_rd <- L_d %*% eta_rd
for (r in 1:nrow(beta_rd)) {
  beta_rd[r,] <- cumsum(beta_rd[r,])
}

# aggregate states are weighted avg of state params
beta_sd <- beta_rd
for (r in 1:nrow(wt)) {
  beta_sd <- rbind(beta_sd, colSums(beta_rd * wt[r,]))
}

# pollster bias
set.seed(6)
n_pollsters <- 8
beta_p <- rnorm(n_pollsters, 0, 0.075)

# group (population) bias
set.seed(7)
n_groups <- 2
beta_g <- rnorm(n_groups, 0, 0.05)

# mode bias
beta_m <- c(0, 0.05)

# sponsor/candidate bias
beta_c <- c(0, 0.05, -0.05)

# simulate data ----------------------------------------------------------------

n_polls <- 800

# probability of polling a specific state (based on close-ness)
state_probs <- (abs(expit(t(beta_s + beta_sd)[1,]) - 0.5)^-1)

# probability of poll occurring on a specific day
day_probs <- dnorm(1:179, 180, 60)

# average sample size (based on mode)
lambda <- c(1300, 2600)

# simulate polls!
set.seed(2024)
polls <-
  tibble(poll = 1:n_polls) %>%

  # poll characteristics
  bind_cols(state = sample(1:length(beta_s),
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
  bind_cols(candidate_sponsor = sample(1:3,
                                       size = nrow(.),
                                       replace = TRUE,
                                       prob = c(9, 1, 1))) %>%
  bind_cols(K = rpois(nrow(.), lambda[.$mode])) %>%

  # apply linear model
  mutate(beta_s = beta_s[state],
         beta_sd = pmap_dbl(list(state, day), ~beta_sd[..1, 180 - ..2]),
         beta_p = beta_p[pollster],
         beta_g = beta_g[group],
         beta_m = beta_m[mode],
         beta_c = beta_c[candidate_sponsor]) %>%
  bind_cols(beta_n = rnorm(nrow(.), 0, 0.05)) %>%
  mutate(mu = beta_s + beta_sd + beta_p + beta_g + beta_m + beta_c + beta_n,
         theta = expit(mu)) %>%

  # sample responses
  bind_cols(Y = rbinom(nrow(.), .$K, .$theta)) %>%
  select(state,
         day,
         K,
         Y,
         pid = pollster,
         gid = group,
         mid = mode,
         cid = candidate_sponsor)

# prep for modeling ------------------------------------------------------------

stan_data <-
  list(
    N = nrow(polls),
    D = 180,
    R = length(population),
    A = max(polls$state) - length(population),
    S = max(polls$state),
    G = max(polls$gid),
    M = max(polls$mid),
    C = max(polls$cid),
    P = max(polls$pid),
    did = polls$day,
    sid = polls$state,
    gid = polls$gid,
    mid = polls$mid,
    cid = polls$cid,
    pid = polls$pid,
    F_r = distances,
    wt = wt,
    K = polls$K,
    Y = polls$Y,
    beta_g_sigma = 0.05,
    beta_m_sigma = 0.05,
    beta_c_sigma = 0.05,
    sigma_n_sigma = 0.05,
    sigma_p_sigma = 0.075,
    e_day_mu_r = e_day_mu,
    e_day_sigma_r = rep(1, n_states),
    rho_alpha = 3,
    rho_beta = 6,
    alpha_sigma = 0.05,
    phi_sigma = 0.05,
    prior_check = 0
  )

poll_model <-
  cmdstan_model("stan/poll_model.stan")

poll_fit <-
  poll_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    init = 0.01,
    step_size = 0.002
  )

# blegh ------------------------------------------------------------------------

truth <-
  t(beta_s + beta_sd) %>%
  expit() %>%
  as_tibble() %>%
  rowid_to_column("day") %>%
  mutate(day = 181 - day) %>%
  pivot_longer(-day,
               names_to = "state",
               values_to = "truth") %>%
  mutate(state = as.integer(parse_number(state))) %>%
  arrange(day, state)

tmp <- poll_fit$summary("theta")

checks <- c(2, 11, 7, 8, 10, 20)
checks <- 1:20
tmp %>%
  mutate(variable = str_remove_all(variable, "theta\\[|\\]")) %>%
  separate(variable, c("state", "day"), ",") %>%
  mutate(across(c(state, day), as.integer)) %>% filter(state %in% checks) %>%
  left_join(truth) %>%
  ggplot(aes(x = day,
             y = median)) +
  # geom_point(data = polls %>% filter(state %in% checks),
  #            mapping = aes(x = day,
  #                          y = Y/K,
  #                          size = K),
  #            shape = 21,
  #            alpha = 0.125) +
  geom_ribbon(aes(ymin = q5,
                  ymax = q95),
              alpha = 0.25,
              fill = "royalblue") +
  geom_line(color = "royalblue") +
  geom_line(aes(y = truth),
            color = "gray40",
            linewidth = 0.25) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  facet_wrap(~state, ncol = (if (length(checks) <= 3) 1 else NULL)) +
  theme_rieke()

