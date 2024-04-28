# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

# set parameters ---------------------------------------------------------------

# states in this fake shindig
n_states <- 8
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
K_s <- 0.05 * exp(-(distances^2)/(2 * (0.5/max_distances)^2))
L_s <- cholesky_decompose(K_s)

# state parameters
set.seed(4)
eta_s <- rnorm(n_states, 0, 1)
beta_s <- (L_s %*% eta_s)[,1] + e_day_mu

# national env is a weighted average of state params
beta_s <- c(beta_s, sum(beta_s * population)/sum(population))

# random walk covariance matrix
K_d <- K_s * 0.05
L_d <- cholesky_decompose(K_d)

# state by day parameters
# here [s,d] is the DAYS UNTIL E-DAY (rather than day)
set.seed(5)
eta_sd <- matrix(rnorm(180*n_states), nrow = n_states, ncol = 180)
beta_sd <- L_d %*% eta_sd
for (r in 1:nrow(beta_sd)) {
  beta_sd[r,] <- cumsum(beta_sd[r,])
}

# national env is a weighted average of state params
beta_sd <- rbind(beta_sd, colSums(beta_sd * population)/sum(population))

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

n_polls <- 360

# probability of polling a specific state (based on close-ness)
state_probs <- (abs(expit(t(beta_s + beta_sd)[1,]) - 0.5)^-1)[1:length(population)]

# national polls correspond to index 5
state_probs <- c(state_probs, mean(state_probs))

# probability of poll occurring on a specific day
day_probs <- dnorm(1:179, 180, 60)

# average sample size (based on mode)
lambda <- c(1300, 2600)

# simulate polls!
set.seed(2024)
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

# don't deal with composites for now
polls <-
  polls %>%
  filter(state != max(state))

stan_data <-
  list(
    N = nrow(polls),
    P = max(polls$pid),
    G = max(polls$gid),
    M = max(polls$mid),
    C = max(polls$cid),
    S = max(polls$state),
    D = 180,
    pid = polls$pid,
    gid = polls$gid,
    mid = polls$mid,
    cid = polls$cid,
    sid = polls$state,
    did = polls$day,
    F_s = distances,
    K = polls$K,
    Y = polls$Y,
    beta_g_sigma = 0.5,
    beta_m_sigma = 0.5,
    beta_c_sigma = 0.5,
    sigma_n_sigma = 0.5,
    sigma_p_sigma = 0.5,
    rho_alpha = 3,
    rho_beta = 6,
    alpha_sigma = 1,
    phi_sigma = 0.05,
    e_day_mu = logit(c(0.7, 0.55, 0.5, 0.45, 0.75, 0.75, 0.6, 0.5)),
    e_day_sigma = rep(0.15, 8),
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

poll_model$code() |> str_c(collapse = "\n") |> message()

poll_fit$draws("p_rep", format = "df") %>%
  as_tibble() %>%
  select(-c(.chain, .iteration)) %>%
  pivot_longer(-.draw) %>%
  nest(data = -name) %>%
  slice_sample(n = 9) %>%
  unnest(data) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 70) +
  facet_wrap(~name, scales = "free")

tmp <- poll_fit$summary("ppc")

tmp %>%
  ggplot(aes(x = parse_number(variable),
             y = median,
             ymin = q5,
             ymax = q95)) +
  geom_ribbon(alpha = 0.25) +
  geom_line()


tmp <- poll_fit$summary("theta")

tmp %>%
  mutate(variable = str_remove_all(variable, "theta\\[|\\]")) %>%
  separate(variable, c("state", "day"), ",") %>%
  mutate(day = as.integer(day),
         across(c(median, q5, q95), expit)) %>%
  ggplot(aes(x = day,
             y = median)) +
  geom_ribbon(aes(ymin = q5,
                  ymax = q95),
              alpha = 0.25) +
  geom_line(data = (beta_s + beta_sd) %>%
              expit() %>%
              t() %>%
              as_tibble() %>%
              rowid_to_column("day") %>%
              mutate(day = 181 - day) %>%
              pivot_longer(-day,
                           names_to = "state",
                           values_to = "median") %>%
              mutate(state = str_remove(state, "V")),
            color = "royalblue") +
  geom_line() +
  geom_point(data = polls %>% mutate(state = as.character(state)),
             mapping = aes(x = day,
                           y = Y/K,
                           size = K),
             shape = 21,
             alpha = 0.5) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  facet_wrap(~state)

polls %>%
  mutate(p = Y/K) %>%
  beta_interval(Y, K - Y) %>%
  ggplot(aes(x = day,
             y = p,
             size = K)) +
  geom_point(alpha = 0.25,
             shape = 21) +
  # geom_smooth(se = FALSE) +
  scale_y_percent() +
  scale_size_continuous(range = c(1, 4)) +
  facet_wrap(~state) +
  theme_rieke()

# asdlkfjadslkf

test <-
  polls %>%
  filter(state != 9)

stan_data <-
  list(
    N = nrow(test),
    D = 180,
    S = max(test$state),
    G = max(test$gid),
    M = max(test$mid),
    C = max(test$cid),
    P = max(test$pid),
    did = test$day,
    sid = test$state,
    gid = test$gid,
    mid = test$mid,
    cid = test$cid,
    pid = test$pid,
    F_s = distances,
    K = test$K,
    Y = test$Y,
    beta_g_sigma = 0.05,
    beta_m_sigma = 0.05,
    beta_c_sigma = 0.05,
    sigma_n_sigma = 0.05,
    sigma_p_sigma = 0.075,
    e_day_mu = logit(c(0.4, 0.5, 0.9, 0.4, 0.6, 0.6, 0.5, 0.4)),
    e_day_sigma = rep(0.75, 8),
    rho_alpha = 3,
    rho_beta = 6,
    alpha_sigma = 0.05,
    phi_sigma = 0.05,
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
    step_size = 0.002
  )

tmp <- test_fit$summary("theta")

tmp %>%
  mutate(variable = str_remove_all(variable, "theta\\[|\\]")) %>%
  separate(variable, c("state", "day"), ",") %>%
  mutate(across(c(state, day), as.integer)) %>% filter(state %in% c(7, 2)) %>%
  ggplot(aes(x = day,
             y = median)) +
  geom_ribbon(aes(ymin = q5,
                  ymax = q95),
              alpha = 0.25,
              fill = "royalblue") +
  geom_line(color = "royalblue") +
  geom_point(data = test %>% filter(state %in% c(7, 2)),
             mapping = aes(x = day,
                           y = Y/K,
                           size = K),
             shape = 21,
             alpha = 0.5) +
  scale_y_percent() +
  scale_size_continuous(range = c(0, 4)) +
  facet_wrap(~state, ncol = 1) +
  theme_rieke()

