# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
gdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-05-11&revision_date=2023-05-11&nd=1947-01-01")
cpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2023-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-05-11&revision_date=2023-05-11&nd=1947-01-01")

# prep -------------------------------------------------------------------------

max_cpi <- max(cpi$CPIAUCSL)

model_gdp <-
  gdp %>%
  left_join(cpi) %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(gdp_adjust = gdp*max_cpi/cpi,
         gdp_growth = (gdp_adjust - lag(gdp_adjust, n = 4))/lag(gdp_adjust, n = 4),
         quarter = quarter(date),
         year = year(date)) %>%
  filter(quarter == 3) %>%
  select(year, gdp_growth)

model_data <-
  abramovitz %>%
  left_join(model_gdp) %>%
  select(dem,
         rep,
         inc_dem,
         inc_rep,
         inc_party,
         inc_running,
         inc_approval = fte_net_inc_approval,
         third_party = third_party_present,
         gdp_growth,
         total_votes) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_rep == 1 ~ "inc rep running",
                                inc_running == 0 & inc_party == "dem" ~ "inc dem party",
                                TRUE ~ "inc rep party"),
         other = 1 - dem - rep,
         inc_approval = inc_approval/100) %>%
  relocate(other, .after = rep) %>%
  select(-inc_dem, -inc_rep, -inc_running)

pid <-
  model_data %>%
  distinct(inc_party) %>%
  rowid_to_column("pid")

iid <-
  model_data %>%
  distinct(inc_status) %>%
  arrange(inc_status) %>%
  rowid_to_column("iid")

model_data <-
  model_data %>%
  left_join(pid) %>%
  left_join(iid)

# priors -----------------------------------------------------------------------

prob <- c(0.475, 0.475, 0.05)
weight <- 150

gtools::rdirichlet(5000, weight*prob) %>%
  as_tibble() %>%
  rename(dem = V1,
         rep = V2,
         other = V3) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name,
             y = value)) +
  ggdist::stat_histinterval() +
  coord_flip()

# model ------------------------------------------------------------------------

R <-
  model_data %>%
  select(dem, rep, other) %>%
  as.matrix()

prior_dr <- log(weight*prob[1])
prior_other <- log(weight*prob[3])

stan_data <-
  list(
    N = nrow(R),
    R = R,
    prior_dr = prior_dr,
    prior_other = prior_other
  )

model <- cmdstanr::cmdstan_model("dev/02-nat-3pv/nat_3pv_01.stan")

fit <-
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

draws <- fit$draws(variables = "theta", format = "df")

draws %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta"),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = case_match(parameter,
                                "theta[1]" ~ "Democrat",
                                "theta[2]" ~ "Republican",
                                "theta[3]" ~ "Other"),
         fill = case_match(parameter,
                           "Democrat" ~ RColorBrewer::brewer.pal(3, "Set1")[2],
                           "Republican" ~ RColorBrewer::brewer.pal(3, "Set1")[1],
                           "Other" ~ "gray60"),
         parameter = fct_reorder(parameter, value)) %>%
  ggplot(aes(x = parameter,
             y = value,
             fill = fill)) +
  ggdist::stat_histinterval(breaks = seq(from = 0, to = 0.7, by = 0.001)) +
  scale_fill_identity() +
  scale_y_percent() +
  coord_flip() +
  theme_rieke() +
  labs(title = "**Simple mean-only model**",
       x = NULL,
       y = NULL)

ggquicksave("dev/02-nat-3pv/nat_3pv_01.png")

# model 2 ----------------------------------------------------------------------

# re-characterize as a multinomial
R <-
  model_data %>%
  mutate(dem = round(dem*total_votes),
         rep = round(rep*total_votes),
         other = total_votes - dem - rep,
         across(c(dem, rep, other), as.integer)) %>%
  select(dem, rep, other) %>%
  as.matrix()

stan_data <-
  list(
    N = nrow(R),
    R = R,
    prior_dr = prior_dr,
    prior_other = prior_other
  )

model <- cmdstanr::cmdstan_model("dev/02-nat-3pv/nat_3pv_02.stan")

fit <-
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

draws <- fit$draws(variables = "theta", format = "df")

draws %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta"),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = case_match(parameter,
                                "theta[1]" ~ "Democrat",
                                "theta[2]" ~ "Republican",
                                "theta[3]" ~ "Other"),
         fill = case_match(parameter,
                           "Democrat" ~ RColorBrewer::brewer.pal(3, "Set1")[2],
                           "Republican" ~ RColorBrewer::brewer.pal(3, "Set1")[1],
                           "Other" ~ "gray60"),
         parameter = fct_reorder(parameter, value)) %>%
  ggplot(aes(x = parameter,
             y = value,
             fill = fill)) +
  ggdist::stat_histinterval(breaks = seq(from = 0, to = 0.7, by = 0.001)) +
  scale_fill_identity() +
  scale_y_percent() +
  coord_flip() +
  theme_rieke() +
  labs(title = "**Simple mean-only model**",
       subtitle = "Reparameterized as a multinomial model",
       x = NULL,
       y = NULL)

ggquicksave("dev/02-nat-3pv/nat_3pv_02.png")

# model 3 ----------------------------------------------------------------------

# priors
tibble(alpha_1 = rnorm(5000, prior_dr, 0.1),
       alpha_2 = rnorm(5000, prior_dr, 0.1),
       other = prior_other,
       inc_status_12 = rnorm(5000, 0.125, 0.1),
       inc_status_22 = rnorm(5000, -0.125, 0.1)) %>%
  mutate(theta_1 = alpha_1 + inc_status_12,
         theta_2 = alpha_2 + inc_status_22,
         probs = pmap(list(theta_1, theta_2, other),
                      ~softmax(c(..1, ..2, ..3))),
         p_1 = map_dbl(probs, ~.x[1]),
         p_2 = map_dbl(probs, ~.x[2]),
         p_3 = map_dbl(probs, ~.x[3])) %>%
  select(starts_with("p_")) %>%
  pivot_longer(everything(),
               names_to = "candidate",
               values_to = "prob") %>%
  ggplot(aes(x = candidate,
             y = prob)) +
  ggdist::stat_histinterval() +
  coord_flip()

x <-
  model_data %>%
  mutate(alpha = 1) %>%
  select(alpha,
         third_party) %>%
  as.matrix()


stan_data <-
  list(
    N = nrow(R),
    K = 3,
    D = ncol(x),
    R = R,
    x = x
  )

model <- cmdstanr::cmdstan_model("dev/02-nat-3pv/nat_3pv_03.stan")

fit <-
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

draws <- fit$draws(variables = "prob", format = "df")

fit$summary("prob") %>%
  select(variable,
         .pred = median) %>%
  mutate(variable = str_remove_all(variable, "prob\\[|\\]")) %>%
  separate(variable, c("N", "party"), ",") %>%
  mutate(across(c(N, party), as.integer),
         party = case_match(party,
                            1 ~ "dem",
                            2 ~ "rep",
                            3 ~ "other")) %>%
  left_join(model_data %>%
              rowid_to_column("N") %>%
              select(N, dem, rep, other) %>%
              pivot_longer(-N,
                           names_to = "party",
                           values_to = "pct")) %>%
  ggplot(aes(x = pct,
             y = .pred,
             color = party)) +
  geom_abline(linetype = "dashed",
              color = "gray60") +
  geom_point(size = 3) +
  scale_xy_percent() +
  NatParksPalettes::scale_color_natparks_d("Triglav") +
  theme_rieke() +
  labs(title = "**Multinomial model with indicator for 3rd party**",
       subtitle = "Probs need to switch back to Dirichlet mod wiht smart priors")

ggquicksave("dev/02-nat-3pv/nat_3pv_03.png")

# model w/multiple predictors --------------------------------------------------

# reparameterize back to dirichlet
R <-
  model_data %>%
  select(dem, rep, other) %>%
  as.matrix()

stan_data <-
  list(
    N = nrow(R),
    K = 3,
    N_inc_party = max(model_data$pid),
    N_inc_status = max(model_data$iid),
    R = R,
    iid = model_data$iid,
    pid = model_data$pid
  )

model <- cmdstanr::cmdstan_model("dev/02-nat-3pv/nat_3pv_04.stan")

fit <-
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

draws <- fit$draws(variables = "prob", format = "df")

draws %>%
  as_tibble() %>%
  pivot_longer(starts_with("prob"),
               values_to = "pct") %>%
  mutate(name = str_remove_all(name, "prob\\[|\\]")) %>%
  separate(name, c("rowid", "party")) %>%
  mutate(across(c(rowid, party), as.integer),
         party = case_match(party,
                            1 ~ "dem",
                            2 ~ "rep",
                            3 ~ "other")) %>%
  group_by(party, rowid) %>%
  tidybayes::median_qi(pct, .width = c(0.5, 0.66, 0.95)) %>%
  left_join(model_data %>%
              select(dem, rep, other) %>%
              rowid_to_column() %>%
              pivot_longer(c(dem, rep, other),
                           names_to = "party",
                           values_to = "pct_actual"),
            by = c("rowid", "party")) %>%
  ggplot(aes(x = pct_actual,
             y = pct,
             ymin = .lower,
             ymax = .upper,
             color = party)) +
  geom_abline(linetype = "dashed",
              color = "gray40") +
  ggdist::geom_pointinterval(alpha = 0.25) +
  NatParksPalettes::scale_color_natparks_d("Triglav") +
  scale_xy_percent() +
  theme_rieke() +
  labs(title = "**Multiple predictors, poor priors**")

ggquicksave("dev/02-nat-3pv/nat_3pv_04.png")

