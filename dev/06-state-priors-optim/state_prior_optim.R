# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(patchwork)

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
states <- read_csv("data/statewide_results.csv")
approval <- read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv")
electors <- read_csv("data/electors.csv")
centers <- read_csv("data/state_centers.csv")

# prep stan data ---------------------------------------------------------------

# wrangle training data
model_data <-
  abramovitz %>%
  select(dem,
         rep,
         inc_dem,
         inc_rep,
         inc_party,
         inc_running,
         inc_approval = fte_net_inc_approval,
         third_party = third_party_present) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_rep == 1 ~ "inc rep running",
                                inc_running == 0 & inc_party == "dem" ~ "inc dem party",
                                TRUE ~ "inc rep party"),
         other = 1 - dem - rep,
         inc_approval = inc_approval/100) %>%
  relocate(other, .after = rep) %>%
  select(-inc_dem, -inc_rep, -inc_running)

# get into a format stan will accept
iid <-
  model_data %>%
  distinct(inc_status) %>%
  arrange(inc_status) %>%
  rowid_to_column("iid")

model_data <-
  model_data %>%
  left_join(iid)

R <-
  model_data %>%
  select(dem, rep, other) %>%
  as.matrix()

# pull in biden's current approval for prior prediction
biden_approval <-
  approval %>%
  filter(subgroup == "All polls",
         end_date < mdy("11/5/24")) %>%
  filter(end_date == max(end_date)) %>%
  mutate(net = approve_estimate - disapprove_estimate,
         net = net/100) %>%
  pull(net)

# model ------------------------------------------------------------------------

n_samples <- 8000

stan_data <-
  list(
    R = R,
    N = nrow(R),
    K = ncol(R),
    N_inc_status = max(model_data$iid),
    iid = model_data$iid,
    inc_approval = model_data$inc_approval,
    third_party = model_data$third_party,
    biden_approval = biden_approval
  )

national_prior_model <-
  cmdstanr::cmdstan_model("dev/06-state-priors-optim/national_prior.stan")

national_prior_fit <-
  national_prior_model$sample(
    data = stan_data,
    seed = 2024,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = n_samples/4,
    iter_sampling = n_samples/4
  )

# set state priors -------------------------------------------------------------

# extract model's national prior
national_prior <-
  national_prior_fit$draws(variables = "prob_gq", format = "df") %>%
  as_tibble() %>%
  select(starts_with("prob")) %>%
  rename_with(~as.character(parse_number(.x))) %>%
  rename_with(~case_match(as.numeric(.x),
                          1 ~ "dem",
                          2 ~ "rep",
                          3 ~ "oth")) %>%
  rowid_to_column(".draw") %>%
  nest(national_draws = everything())

state_pvi <-

  # join state/national data together
  states %>%
  select(year,
         state,
         democratic,
         republican,
         other) %>%
  mutate(across(-c(year, state), ~.x/100)) %>%
  left_join(abramovitz %>%
              select(year,
                     nat_dem = dem,
                     nat_rep = rep)) %>%

  # get election-specific 3pvi
  mutate(nat_oth = 1 - (nat_dem + nat_rep),
         pvi_3d = democratic - nat_dem,
         pvi_3r = republican - nat_rep,
         pvi_3o = other - nat_oth) %>%

  # get the forward looking 3pvi
  group_by(state) %>%
  arrange(year) %>%
  mutate(year = year + 4,
         across(starts_with("pvi"), ~0.75*.x + 0.25*lag(.x, 1))) %>%
  ungroup() %>%
  filter(year == 2024) %>%
  select(state, starts_with("pvi"))

state_priors <-
  state_pvi %>%
  mutate(national_prior) %>%
  unnest(national_draws) %>%
  mutate(dem = dem + pvi_3d,
         rep = rep + pvi_3r,
         oth = oth + pvi_3o,
         across(c(dem, rep, oth), ~case_when(.x < 0 ~ 0,
                                             .x > 1 ~ 1,
                                             TRUE ~ .x)),
         across(c(dem, rep, oth), ~.x/(dem + rep + oth))) %>%
  select(.draw, state, dem, rep, oth)

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

# extract parameters from state priors -----------------------------------------

state_priors <-
  state_priors %>%
  mutate(weight = pmap_dbl(list(dem, rep, oth), ~estimate_parameters(c(..1, ..2, ..3))),
         prior_dem = dem*weight,
         prior_rep = rep*weight)

# random sample of parameter space ---------------------------------------------

state_priors %>%
  select(state, starts_with("prior")) %>%
  pivot_longer(starts_with("prior"),
               names_to = "party",
               values_to = "parameter") %>%
  nest(data = -state) %>%
  slice_sample(n = 12) %>%
  unnest(data) %>%
  ggplot(aes(x = parameter,
             fill = party)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(values = c("royalblue", "red")) +
  facet_wrap(~state, scales = "free") +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior parameters for a sample of states**",
       subtitle = "Model based on incumbency and approval",
       x = NULL,
       y = NULL)

ggquicksave("dev/06-state-priors-optim/state_prior_optim_01.png")

# state margin plot ------------------------------------------------------------

state_priors %>%
  mutate(delta = dem - rep) %>%
  group_by(state) %>%
  mutate(dem_win = if_else(delta > 0, 1, 0),
         dem_win = sum(dem_win)/n_samples,
         state = paste(state, scales::label_percent(accuracy = 1)(dem_win))) %>%
  tidybayes::median_qi(delta, .width = c(0.5, 0.8, 0.95)) %>%
  mutate(state = fct_reorder(state, delta)) %>%
  ggplot(aes(x = state,
             y = delta,
             ymin = .lower,
             ymax = .upper,
             color = delta)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 0.5) +
  ggdist::geom_pointinterval() +
  scale_color_gradientn(colors = NatParksPalettes::NatParksPalettes$Triglav[[1]][3:1],
                        limits = c(-1, 1)) +
  scale_y_percent() +
  coord_flip() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior model of Democratic Margin**",
       subtitle = glue::glue("Based on an incumbent net approval of **",
                             "{scales::label_percent(accuracy = 0.1)(biden_approval)}",
                             "**"),
       x = NULL,
       y = NULL,
       caption = glue::glue("Posterior interval based on {scales::label_comma()(n_samples)} MCMC samples",
                            "Pointwidths indicate 50/80/95% credible intervals",
                            .sep = "<br>"))

ggquicksave("dev/06-state-priors-optim/state_prior_optim_02.png",
            width = 9,
            height = 15)

# electoral college distribution -----------------------------------------------

ec_axis <-
  tibble(breaks = seq(0, 538, length.out = 11)) %>%
  mutate(color = case_when(breaks > 269 ~ "#B13737",
                           breaks < 269 ~ "#3579AC",
                           TRUE ~ "gray60"),
         labels = case_when(breaks >= 269 ~ round(breaks),
                            breaks < 269 ~ 538 - round(breaks)),
         labels = color_text(labels, color))

ec_counts <-
  state_priors %>%
  left_join(electors) %>%
  mutate(winner = case_when(dem > rep & dem > oth ~ "dem",
                            rep > dem & rep > oth ~ "rep",
                            TRUE ~ "oth")) %>%
  group_by(.draw, winner) %>%
  summarise(electors = sum(electors)) %>%
  ungroup() %>%
  filter(winner == "rep") %>%
  mutate(winner = case_when(electors >= 270 ~ "rep",
                            electors == 269 ~ "tie",
                            TRUE ~ "dem"))
biden_prob <-
  ec_counts %>%
  percent(winner) %>%
  filter(winner == "dem") %>%
  pull(pct)

ec_counts %>%
  ggplot(aes(x = electors,
             fill = winner)) +
  geom_histogram(binwidth = 1) +
  geom_hline(yintercept = 0,
             color = "gray60") +
  scale_fill_manual(values = c("#3579AC", "#B13737")) +
  scale_x_continuous(breaks = ec_axis$breaks,
                     labels = ec_axis$labels) +
  theme_rieke() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = ggtext::element_markdown(color = "gray20"),
        panel.grid = element_blank()) +
  expand_limits(x = c(0, 538)) +
  labs(title = "**Prior model of Electoral College results**",
       subtitle = glue::glue("Based on a net approval of **",
                             scales::label_percent(accuracy = 0.1)(biden_approval),
                             "**, Biden has a **",
                             scales::label_percent(accuracy = 1)(biden_prob),
                             "** chance of winning re-election"),
       x = NULL,
       y = NULL,
       caption = glue::glue("Distribution of possible electoral college results",
                            "based on {scales::label_comma()(n_samples)} MCMC samples",
                            .sep = "<br>"))

ggquicksave("dev/06-state-priors-optim/state_prior_optim_03.png")

# electoral college map --------------------------------------------------------

# modify theme void w/new font
theme_void_mod <- function() {

  theme_void() +
    theme(plot.title = ggtext::element_markdown(family = "Tiempos Text",
                                                color = "gray20",
                                                size = 18))

}

# data feeding into bar underneath map
ec_bar <-
  state_priors %>%
  mutate(winner = if_else(dem > rep, "dem", "rep")) %>%
  group_by(state) %>%
  percent(winner) %>%
  ungroup() %>%
  mutate(rating = case_when(pct > 0.99 ~ paste("safe", winner),
                            pct > 0.85 ~ paste("very likely", winner),
                            pct > 0.65 ~ paste("likely", winner),
                            pct > 0.50 ~ "uncertain")) %>%
  drop_na() %>%
  left_join(electors) %>%
  group_by(rating) %>%
  summarise(electors = sum(electors)) %>%
  mutate(fill = case_match(rating,
                           "safe dem" ~ "#3579ac",
                           "very likely dem" ~ "#7cb0d7",
                           "likely dem" ~ "#d3e5f2",
                           "uncertain" ~ "#f2f2f2",
                           "likely rep" ~ "#f2d5d5",
                           "very likely rep" ~ "#d78080",
                           "safe rep" ~ "#b13737"),
         rating = fct_relevel(rating, c("safe rep",
                                        "very likely rep",
                                        "likely rep",
                                        "uncertain",
                                        "likely dem",
                                        "very likely dem",
                                        "safe dem"))) %>%
  arrange(rating)

# plot of bar
current_bar <-
  ec_bar %>%
  arrange(desc(rating)) %>%
  mutate(label_position = cumsum(electors) - 0.5*electors) %>%
  ggplot() +
  geom_bar(aes(x = 0,
               y = electors,
               fill = rating),
           position = "stack",
           stat = "identity") +
  geom_text(aes(label = electors,
                x = 0,
                y = label_position),
            family = "IBM Plex Sans",
            fontface = "bold",
            size = 4) +
  scale_fill_manual(values = ec_bar$fill) +
  expand_limits(x = c(-1, 1)) +
  coord_flip() +
  theme_void_mod() +
  theme(legend.position = "none")

# create map w/points for state rating/number of electors
us_geo <-
  tigris::states(class = "sf", cb = TRUE) %>%
  tigris::shift_geometry() %>%
  filter(GEOID < 60)

map_electors <-
  state_priors %>%
  mutate(winner = if_else(dem > rep, "dem", "rep")) %>%
  group_by(state) %>%
  percent(winner) %>%
  ungroup() %>%
  mutate(rating = case_when(pct > 0.99 ~ paste("safe", winner),
                            pct > 0.85 ~ paste("very likely", winner),
                            pct > 0.65 ~ paste("likely", winner),
                            pct > 0.50 ~ "uncertain")) %>%
  drop_na() %>%
  left_join(electors) %>%
  mutate(fill = case_match(rating,
                           "safe dem" ~ "#3579ac",
                           "very likely dem" ~ "#7cb0d7",
                           "likely dem" ~ "#d3e5f2",
                           "uncertain" ~ "#f2f2f2",
                           "likely rep" ~ "#f2d5d5",
                           "very likely rep" ~ "#d78080",
                           "safe rep" ~ "#b13737")) %>%
  left_join(centers) %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326)


current_map <-
  us_geo %>%
  left_join(electors, by = c("NAME" = "state")) %>%
  ggplot() +
  geom_sf(fill = "#fafafa",
          color = "#ededed",
          linewidth = 0.5) +
  geom_sf(data = map_electors,
          aes(size = electors,
              fill = fill,
              color = if_else(rating == "uncertain", "black", "white")),
          shape = 21) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_size_continuous(range = c(3, 12)) +
  theme_void_mod() +
  theme(legend.position = "none")

current_map

ggquicksave("dev/06-state-priors-optim/state_prior_optim_04.png")

# legend at the top
ec_ratings <-
  ec_bar %>%
  rowid_to_column() %>%
  arrange(desc(rowid)) %>%
  select(-rowid) %>%
  rowid_to_column() %>%
  mutate(rating = str_to_title(rating),
         color = if_else(rating == "Uncertain", "black", "white")) %>%
  ggplot(aes(x = rowid,
             y = 0,
             fill = fill,
             color = color,
             label = rating)) +
  geom_point(shape = 21,
             size = 4) +
  geom_text(color = "gray20",
            family = "IBM Plex Sans",
            fontface = "bold",
            size = 3,
            vjust = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void_mod() +
  labs(title = "**Prior Electoral College Ratings**") +
  expand_limits(x = c(0.5, 7.5))

(ec_ratings / current_map / current_bar) +
  plot_layout(heights = c(1, 9, 1))

ggquicksave("dev/06-state-priors-optim/state_prior_optim_05.png",
            width = 7.2,
            height = 6.2)

# more dev ---------------------------------------------------------------------

state_priors %>%
  pivot_longer(starts_with("prior"),
               names_to = "party",
               values_to = "parameter") %>%
  mutate(party = str_remove(party, "prior_")) %>%
  group_by(state, party) %>%
  summarise(prior_mean = mean(parameter),
            prior_sd = sd(parameter)) %>%
  ungroup()
