# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

model_data <-
  read_csv("dev/07-abramovitz-3pv/model_data.csv")

# prior sim --------------------------------------------------------------------

sims <- 1000
sigma <- 0.075

even <- 0.475
inc_bonus <- 0.025
wt <- 2

inc_app_base <- 0.1
inc_app_run <- 0.125
inc_app_party <- 0.05

gdp_base <- 0.05
gdp_run <- 0.075
gdp_party <- 0.025

# simulate prior parameters
tibble(alpha_1 = rnorm(sims, even*wt, sigma),
       alpha_2 = rnorm(sims, even*wt, sigma),
       inc_run_1 = rnorm(sims, 0.1, sigma),
       inc_run_2 = rnorm(sims, -0.1, sigma),
       inc_not_1 = rnorm(sims, -0.05, sigma),
       inc_not_2 = rnorm(sims, 0.05, sigma),
       inc_app_1 = rnorm(sims, 0.05, sigma),
       inc_app_2 = rnorm(sims, -0.05, sigma),
       gdp_1 = rnorm(sims, 0.05, sigma),
       gdp_2 = rnorm(sims, -0.05, sigma),
       third_party_1 = rnorm(sims, -0.15, sigma),
       third_party_2 = rnorm(sims, -0.15, sigma)) %>%

  # apply sum-to-zero constraint
  mutate(alpha_3 = -(alpha_1 + alpha_2),
         inc_run_3 = -(inc_run_1 + inc_run_2),
         inc_not_3 = -(inc_not_1 + inc_not_2),
         inc_app_3 = -(inc_app_1 + inc_app_2),
         gdp_3 = -(gdp_1 + gdp_2),
         third_party_3 = -(third_party_1 + third_party_2)) %>%

  # join with training data
  nest(data = everything()) %>%
  bind_cols(model_data, .) %>%
  rowid_to_column("election") %>%
  unnest(data) %>%

  # index inc_run/inc_not param
  mutate(inc_status_1 = if_else(inc_running == 1, inc_run_1, inc_not_1),
         inc_status_2 = if_else(inc_running == 1, inc_run_2, inc_not_2),
         inc_status_3 = if_else(inc_running == 1, inc_run_3, inc_not_3)) %>%

  # apply linear model
  mutate(phi_1 = alpha_1 + inc_status_1 + inc_app_1*inc_approval + gdp_1*real_gdp_growth + third_party_1*third_party,
         phi_2 = alpha_2 + inc_status_2 + inc_app_2*inc_approval + gdp_2*real_gdp_growth + third_party_2*third_party,
         phi_3 = alpha_3 + inc_status_3 + inc_app_3*inc_approval + gdp_3*real_gdp_growth + third_party_3*third_party) %>%
  select(election, starts_with("phi")) %>%

  # convert lm to probabilities
  mutate(.preds = pmap(list(phi_1, phi_2, phi_3),
                       ~softmax(c(..1, ..2, ..3))),
         .pred_inc = map_dbl(.preds, ~.x[1]),
         .pred_non = map_dbl(.preds, ~.x[2]),
         .pred_oth = map_dbl(.preds, ~.x[3])) %>%
  select(election, starts_with(".pred_")) %>%

  # estimate med/lower/upper
  pivot_longer(starts_with(".pred_"),
               names_to = "party",
               values_to = "pct") %>%
  mutate(party = str_remove(party, ".pred_")) %>%
  group_by(election, party) %>%
  summarise(.pred = quantile(pct, probs = 0.5),
            .pred_lower = quantile(pct, probs = 0.025),
            .pred_upper = quantile(pct, probs = 0.975)) %>%
  ungroup() %>%

  # join with election results
  left_join(model_data %>%
              bind_cols(abramovitz %>% select(year)) %>%
              select(year, ends_with(("pct"))) %>%
              rowid_to_column("election") %>%
              pivot_longer(ends_with("pct"),
                           names_to = "party",
                           values_to = "result") %>%
              mutate(party = str_sub(party, 1, 3))) %>%

  # plot!
  ggplot(aes(x = year,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper,
             color = party)) +
  geom_ribbon(aes(fill = party,
                  color = NULL),
              alpha = 0.25) +
  geom_line() +
  geom_point(aes(y = result)) +
  scale_y_percent() +
  NatParksPalettes::scale_fill_natparks_d("Charmonix") +
  NatParksPalettes::scale_color_natparks_d("Charmonix") +
  theme_rieke() +
  labs(title = "**Prior model of incumbent voteshare**",
       subtitle = paste("Based on incumbency, net approval, real gdp growth",
                        "and the presence of a third party on the ballot",
                        sep = "<br>"),
       x = NULL,
       y = NULL)

ggquicksave("dev/07-abramovitz-3pv/abramovitz_3pv_prior.png")








