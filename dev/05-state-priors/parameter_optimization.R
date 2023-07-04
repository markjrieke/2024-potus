# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# simulate data ----------------------------------------------------------------

dem <- rnorm(1000, 1.25, 0.0625)
rep <- rnorm(1000, 0.75, 0.0625)

sims <-
  tibble(dem, rep) %>%
  mutate(oth = -(dem + rep),
         prob = pmap(list(dem, rep, oth), ~c(..1, ..2, ..3)),
         prob = map(prob, softmax),
         dem = map_dbl(prob, ~.x[1]),
         rep = map_dbl(prob, ~.x[2]),
         oth = map_dbl(prob, ~.x[3])) %>%
  select(-prob)

sims %>%
  pivot_longer(everything(),
               names_to = "party",
               values_to = "pct") %>%
  ggplot(aes(x = pct,
             fill = party)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 breaks = seq(from = 0,
                              to = 0.75,
                              length.out = 100)) +
  scale_x_percent() +
  NatParksPalettes::scale_fill_natparks_d("Triglav") +
  theme_rieke()

ggquicksave("dev/05-state-priors/param_optimization_01.png")

# functions for optimizing error -----------------------------------------------

weight_error <- function(pct_vector, weight = 2) {

  phi <- weight*pct_vector[1:2]
  phi <- c(phi, -sum(phi))
  prob <- softmax(phi)
  err <- sqrt(sum((pct_vector - prob)^2)/3)

  return(err)

}

estimate_parameters <- function(pct_vector) {

  weight <- optimise(weight_error, interval = c(0.1, 10), pct_vector = pct_vector)

  return(weight$minimum)

}

# optimization output ----------------------------------------------------------

param_recovery <-
  sims %>%
  mutate(predictions = pmap(list(dem, rep, oth), ~c(..1, ..2, ..3)),
         weight = map_dbl(predictions, estimate_parameters),
         dem_optim = weight*dem,
         rep_optim = weight*rep,
         oth_optim = -(dem_optim + rep_optim),
         optim_preds = pmap(list(dem_optim, rep_optim, oth_optim), ~softmax(c(..1, ..2, ..3))),
         dem_optim = map_dbl(optim_preds, ~.x[1]),
         rep_optim = map_dbl(optim_preds, ~.x[2]),
         oth_optim = map_dbl(optim_preds, ~.x[3])) %>%
  select(-predictions, -optim_preds, -weight) %>%
  rowid_to_column()

actuals <-
  param_recovery %>%
  select(-ends_with("optim")) %>%
  pivot_longer(-rowid,
               names_to = "party",
               values_to = "actual")

param_recovery %>%
  select(rowid, ends_with("optim")) %>%
  pivot_longer(-rowid,
               names_to = "party",
               values_to = "optim") %>%
  mutate(party = str_remove(party, "_optim")) %>%
  left_join(actuals) %>%
  pivot_longer(c(optim, actual),
               names_to = "estimator",
               values_to = "value") %>%
  ggplot(aes(x = value,
             color = estimator)) +
  geom_density() +
  scale_x_percent() +
  facet_wrap(~party, scales = "free_y") +
  theme_rieke()

ggquicksave("dev/05-state-priors/param_optimization_02.png")

# error function ---------------------------------------------------------------

tibble(weight = seq(from = 0.1, to = 10, length.out = 100)) %>%
  mutate(error = map_dbl(weight, ~weight_error(c(0.50, 0.45, 0.05), .x))) %>%
  ggplot(aes(x = weight,
             y = error)) +
  geom_line() +
  theme_rieke() +
  labs(title = "**Example Error Function**",
       subtitle = paste("A weight of about **1.6** minimizes the error",
                        "for a 50/45/5% split D/R/O",
                        sep = "<br>"))

ggquicksave("dev/05-state-priors/param_optimization_03.png")
