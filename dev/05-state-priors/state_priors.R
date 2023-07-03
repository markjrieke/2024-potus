# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# fred functions ---------------------------------------------------------------

fetch_cpi <- function() {

  current_date <- Sys.Date()
  last_month <- floor_date(floor_date(current_date, "month") - 1, "month")

  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=",
        last_month,
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )

  return(out)

}

fetch_gdp <- function() {

  current_date <- Sys.Date()

  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )

  return(out)

}

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
states <- read_csv("data/statewide_results.csv")
gdp <- fetch_gdp()
cpi <- fetch_cpi()

# wrangle economic data --------------------------------------------------------

max_cpi <-
  cpi %>%
  filter(DATE == max(DATE)) %>%
  pull(CPIAUCSL)

cpi <-
  cpi %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(inflation = max_cpi/cpi)

# adjust gdp for inflation
real_gdp <-
  gdp %>%
  rename_with(str_to_lower) %>%
  left_join(cpi) %>%
  select(-cpi) %>%
  mutate(real_gdp = gdp*inflation) %>%
  select(date, real_gdp)

# append national data with real gdp growth
abramovitz <-
  abramovitz %>%
  mutate(begin_quarter = mdy(paste0("7/1/", year - 1)),
         end_quarter = mdy(paste0("7/1/", year))) %>%
  left_join(real_gdp, by = c("begin_quarter" = "date")) %>%
  rename(begin_gdp = real_gdp) %>%
  left_join(real_gdp, by = c("end_quarter" = "date")) %>%
  rename(end_gdp = real_gdp) %>%
  mutate(real_gdp_growth = end_gdp/begin_gdp - 1) %>%
  select(-ends_with("quarter"),
         -ends_with("gdp"))

# quick plot!
abramovitz %>%
  ggplot(aes(x = year,
             y = real_gdp_growth,
             fill = inc_party)) +
  geom_col(alpha = 0.75) +
  scale_fill_manual(values = c("royalblue", "red")) +
  scale_y_percent() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**2nd Quarter Annualized Real GDP Growth**",
       subtitle = glue::glue("In election years with an incumbent ",
                             "**{color_text('Democrat', 'royalblue')}** or ",
                             "**{color_text('Republican', 'red')}**"),
       x = NULL,
       y = NULL)

ggquicksave("dev/05-state-priors/real_gdp_growth.png")

# wrangle state data -----------------------------------------------------------

# get pvi of each state for a given year
state_results <-
  states %>%
  select(-notes) %>%
  mutate(across(democratic:other, ~.x/100)) %>%
  left_join(abramovitz %>% select(year, dem, rep),
            by = "year") %>%
  mutate(oth = 1 - (dem + rep),
         pvi_3d = democratic - dem,
         pvi_3r = republican - rep,
         pvi_3o = other - oth) %>%
  select(-c(dem, rep, oth))

# get the average pvi for the first two elections each state appears in
first_state_pvi  <-
  state_results %>%
  group_by(state) %>%
  arrange(year) %>%
  slice_head(n = 2) %>%
  summarise(first_pvi_3d = mean(pvi_3d),
            first_pvi_3r = mean(pvi_3r),
            first_pvi_3o = mean(pvi_3o))

# add in lagged pvi for each state
state_results <-
  state_results %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(pvi_3d = 0.75*lag(pvi_3d, 1) + 0.25*lag(pvi_3d, 2),
         pvi_3r = 0.75*lag(pvi_3r, 1) + 0.25*lag(pvi_3r, 2),
         pvi_3o = 0.75*lag(pvi_3o, 1) + 0.25*lag(pvi_3o, 2)) %>%
  ungroup() %>%
  left_join(first_state_pvi, by = "state") %>%
  mutate(pvi_3d = if_else(is.na(pvi_3d), first_pvi_3d, pvi_3d),
         pvi_3r = if_else(is.na(pvi_3r), first_pvi_3r, pvi_3r),
         pvi_3o = if_else(is.na(pvi_3o), first_pvi_3o, pvi_3o)) %>%
  select(-starts_with("first"))

# join with national data
state_results <-
  abramovitz %>%
  select(year,
         inc_party,
         inc_dem,
         inc_rep,
         inc_approval = fte_net_inc_approval,
         real_gdp_growth) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_dem == 0 & inc_party == "dem" ~ "inc dem party",
                                inc_rep == 1 ~ "inc rep running",
                                inc_rep == 0 & inc_party == "rep" ~ "inc rep party"),
         inc_approval = inc_approval/100) %>%
  select(year, inc_approval, inc_status, real_gdp_growth) %>%
  right_join(state_results, by = "year")

# prep for modeling ------------------------------------------------------------

state_results <-
  state_results %>%
  mutate(iid = case_match(inc_status,
                          "inc dem running" ~ 1,
                          "inc dem party" ~ 2,
                          "inc rep running" ~ 3,
                          "inc rep party" ~ 4),
         iid = as.integer(iid),
         other = if_else(other == 0, 0.001, other),
         across(c(democratic, republican, other),
                ~.x/(democratic + republican + other)))

R <-
  state_results %>%
  select(democratic, republican, other) %>%
  as.matrix()

# prior model 01 ---------------------------------------------------------------

prior_data <-
  list(
    N = nrow(R),
    K = ncol(R),
    R = R,
    N_inc_status = 4,
    iid = state_results$iid
  )

prior_model_01 <-
  cmdstanr::cmdstan_model("dev/05-state-priors/state_priors_01.stan")

prior_fit_01 <-
  prior_model_01$sample(
    data = prior_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

# prior model 01 predictions ---------------------------------------------------

plot_predictions <- function(fit) {

  fit$draws("prob", format = "df") %>%
    as_tibble() %>%
    pivot_longer(starts_with("prob"),
                 names_to = "variable",
                 values_to = "pct") %>%
    group_by(variable) %>%
    summarise(.pred = quantile(pct, probs = 0.5),
              .pred_lower = quantile(pct, probs = 0.1),
              .pred_upper = quantile(pct, probs = 0.9)) %>%
    separate(variable, c("rowid", "party"), ",") %>%
    mutate(rowid = as.integer(str_sub(rowid, 6)),
           party = as.integer(str_sub(party, 1, 1)),
           party = case_match(party,
                              1 ~ "democratic",
                              2 ~ "republican",
                              3 ~ "other")) %>%
    left_join(state_results %>%
                select(democratic, republican, other) %>%
                rowid_to_column() %>%
                pivot_longer(c(democratic, republican, other),
                             names_to = "party",
                             values_to = "result")) %>%
    ggplot(aes(x = result,
               y = .pred,
               ymin = .pred_lower,
               ymax = .pred_upper,
               color = party)) +
    geom_pointrange(alpha = 0.25) +
    geom_abline(linetype = "dashed",
                linewidth = 0.25) +
    scale_xy_percent() +
    NatParksPalettes::scale_color_natparks_d("Triglav") +
    theme_rieke() +
    theme(legend.position = "none")

}

prior_fit_01 %>%
  plot_predictions() +
  labs(title = "**State Prior Model 01**",
       subtitle = "Based on incumbency",
       x = "Result",
       y = "Prediction",
       caption = "Based on 2,000 MCMC samples")

ggquicksave("dev/05-state-priors/state_prior_pred_01.png")

# prior model 02 ---------------------------------------------------------------

prior_data <-
  list(
    N = nrow(R),
    K = ncol(R),
    R = R,
    N_inc_status = 4,
    iid = state_results$iid,
    pvi_3d = rethinking::standardize(state_results$pvi_3d),
    pvi_3r = rethinking::standardize(state_results$pvi_3r)
  )

prior_model_02 <-
  cmdstanr::cmdstan_model("dev/05-state-priors/state_priors_02.stan")

prior_fit_02 <-
  prior_model_02$sample(
    data = prior_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

prior_fit_02 %>%
  plot_predictions() +
  labs(title = "**State Prior Model 02**",
       subtitle = "Based on incumbency and partisanship",
       x = "Result",
       y = "Prediction",
       caption = "Based on 2,000 MCMC samples")

ggquicksave("dev/05-state-priors/state_prior_pred_02.png")

# prior model 03 ---------------------------------------------------------------

prior_data <-
  list(
    N = nrow(R),
    K = ncol(R),
    R = R,
    N_inc_status = 4,
    iid = state_results$iid,
    pvi_3d = rethinking::standardize(state_results$pvi_3d),
    pvi_3r = rethinking::standardize(state_results$pvi_3r),
    third_party = state_results$third_party_flag
  )

prior_model_03 <-
  cmdstanr::cmdstan_model("dev/05-state-priors/state_priors_03.stan")

prior_fit_03 <-
  prior_model_03$sample(
    data = prior_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

prior_fit_03 %>%
  plot_predictions() +
  labs(title = "**State Prior Model 03**",
       subtitle = "Based on incumbency, partisanship, and the presence of a third party",
       x = "Result",
       y = "Prediction",
       caption = "Based on 2,000 MCMC samples")

ggquicksave("dev/05-state-priors/state_prior_pred_03.png")

####################### WORK TO DO BRUH START HERE #############################

# prior model ------------------------------------------------------------------

sims <- 1000
sigma <- 0.1

even <- 0.475
inc_bonus <- 0.025
wt <- 2

inc_app_base <- 0.1
inc_app_run <- 0.125
inc_app_party <- 0.05

gdp_base <- 0.05
gdp_run <- 0.075
gdp_party <- 0.025

# sim raw parameters
bind_cols(# incumbency status
          idr_1 = rnorm(sims, (even + inc_bonus)*wt, sigma),
          idr_2 = rnorm(sims, (even - inc_bonus)*wt, sigma),
          idp_1 = rnorm(sims, wt*0.5, sigma),
          idp_2 = rnorm(sims, wt*0.5, sigma),
          irr_1 = rnorm(sims, (even - inc_bonus)*wt, sigma),
          irr_2 = rnorm(sims, (even + inc_bonus)*wt, sigma),
          irp_1 = rnorm(sims, wt*0.5, sigma),
          irp_2 = rnorm(sims, wt*0.5, sigma),

          # 3pvi
          beta_3d1 = rnorm(sims, 0.1, sigma),
          beta_3d2 = rnorm(sims, -0.1, sigma),
          beta_3r1 = rnorm(sims, -0.1, sigma),
          beta_3r2 = rnorm(sims, 0.1, sigma),

          # incumbent approval
          inc_approval_11 = rnorm(sims, inc_app_base + inc_app_run, sigma),
          inc_approval_12 = rnorm(sims, inc_app_base - inc_app_run, sigma),
          inc_approval_21 = rnorm(sims, inc_app_base + inc_app_party, sigma),
          inc_approval_22 = rnorm(sims, inc_app_base - inc_app_party, sigma),
          inc_approval_31 = rnorm(sims, inc_app_base - inc_app_run, sigma),
          inc_approval_32 = rnorm(sims, inc_app_base + inc_app_run, sigma),
          inc_approval_41 = rnorm(sims, inc_app_base - inc_app_party, sigma),
          inc_approval_42 = rnorm(sims, inc_app_base + inc_app_party, sigma),

          # gdp
          gdp_11 = rnorm(sims, gdp_base + gdp_run, sigma),
          gdp_12 = rnorm(sims, gdp_base - gdp_run, sigma),
          gdp_21 = rnorm(sims, gdp_base + gdp_party, sigma),
          gdp_22 = rnorm(sims, gdp_base - gdp_party, sigma),
          gdp_31 = rnorm(sims, gdp_base - gdp_run, sigma),
          gdp_32 = rnorm(sims, gdp_base + gdp_run, sigma),
          gdp_41 = rnorm(sims, gdp_base - gdp_party, sigma),
          gdp_42 = rnorm(sims, gdp_base + gdp_party, sigma),

          # third party presence!
          third_party_1 = rnorm(sims, -0.35, sigma),
          third_party_2 = rnorm(sims, -0.35, sigma)) %>%

  # sum-to-zero constraint on raw parameters
  mutate(idr_3 = -(idr_1 + idr_2),
         idp_3 = -(idp_1 + idp_2),
         irr_3 = -(irr_1 + irr_2),
         irp_3 = -(irp_1 + irp_2),
         beta_3d3 = -(beta_3d1 + beta_3d2),
         beta_3r3 = -(beta_3r1 + beta_3r2),
         inc_approval_13 = -(inc_approval_11 + inc_approval_12),
         inc_approval_23 = -(inc_approval_21 + inc_approval_22),
         inc_approval_33 = -(inc_approval_31 + inc_approval_32),
         inc_approval_43 = -(inc_approval_41 + inc_approval_42),
         gdp_13 = -(gdp_11 + gdp_12),
         gdp_23 = -(gdp_21 + gdp_22),
         gdp_33 = -(gdp_31 + gdp_32),
         gdp_43 = -(gdp_41 + gdp_42),
         third_party_3 = -(third_party_1 + third_party_2)) %>%

  # join to data & select relevant cols for data
  nest(data = everything()) %>%
  bind_cols(state_results %>%
              mutate(across(c(starts_with("pvi")),
                            ~rethinking::standardize(.x))) %>%
              nest(data = -c(inc_approval, real_gdp_growth)) %>%
              mutate(across(c(inc_approval, real_gdp_growth),
                            ~rethinking::standardize(.x))) %>%
              unnest(data),
            parameters = .) %>%
  rowid_to_column() %>%
  select(rowid,
         iid,
         starts_with("pvi"),
         inc_approval,
         real_gdp_growth,
         third_party_flag,
         data) %>%
  unnest(data) %>%

  # select beta_inc_status
  mutate(inc_1 = case_match(iid,
                            1 ~ idr_1,
                            2 ~ idp_1,
                            3 ~ irr_1,
                            4 ~ irp_1),
         inc_2 = case_match(iid,
                            1 ~ idr_2,
                            2 ~ idp_2,
                            3 ~ irr_2,
                            4 ~ irp_2),
         inc_3 = case_match(iid,
                            1 ~ idr_3,
                            2 ~ idp_3,
                            3 ~ irr_3,
                            4 ~ irp_3),
         beta_app_1 = case_match(iid,
                                 1 ~ inc_approval_11,
                                 2 ~ inc_approval_21,
                                 3 ~ inc_approval_31,
                                 4 ~ inc_approval_41),
         beta_app_2 = case_match(iid,
                                 1 ~ inc_approval_12,
                                 2 ~ inc_approval_22,
                                 3 ~ inc_approval_32,
                                 4 ~ inc_approval_42),
         beta_app_3 = case_match(iid,
                                 1 ~ inc_approval_13,
                                 2 ~ inc_approval_23,
                                 3 ~ inc_approval_33,
                                 4 ~ inc_approval_43),
         beta_gdp_1 = case_match(iid,
                                 1 ~ gdp_11,
                                 2 ~ gdp_21,
                                 3 ~ gdp_31,
                                 4 ~ gdp_41),
         beta_gdp_2 = case_match(iid,
                                 1 ~ gdp_12,
                                 2 ~ gdp_22,
                                 3 ~ gdp_32,
                                 4 ~ gdp_42),
         beta_gdp_3 = case_match(iid,
                                 1 ~ gdp_13,
                                 2 ~ gdp_23,
                                 3 ~ gdp_33,
                                 4 ~ gdp_43)) %>%
  select(-starts_with("idr"),
         -starts_with("idp"),
         -starts_with("irr"),
         -starts_with("irp"),
         -starts_with("inc_approval_"),
         -starts_with("gdp")) %>%

  # apply linear model
  mutate(phi_1 = inc_1 + pvi_3d*beta_3d1 + pvi_3r*beta_3r1 + beta_app_1*inc_approval + beta_gdp_1*real_gdp_growth + third_party_flag*third_party_1,
         phi_2 = inc_2 + pvi_3d*beta_3d2 + pvi_3r*beta_3r2 + beta_app_2*inc_approval + beta_gdp_2*real_gdp_growth + third_party_flag*third_party_2,
         phi_3 = inc_3 + pvi_3d*beta_3d3 + pvi_3r*beta_3r3 + beta_app_3*inc_approval + beta_gdp_3*real_gdp_growth + third_party_flag*third_party_3) %>%
  select(rowid, starts_with("phi")) %>%

  # extract individual party probabilities
  mutate(prob = pmap(list(phi_1, phi_2, phi_3), ~softmax(c(..1, ..2, ..3))),
         .pred_dem = map_dbl(prob, ~.x[1]),
         .pred_rep = map_dbl(prob, ~.x[2]),
         .pred_oth = map_dbl(prob, ~.x[3])) %>%
  select(rowid, starts_with(".pred")) %>% left_join(state_results %>% select(year, state) %>% rowid_to_column()) %>% filter(year == 2020) %>% group_by(state) %>% mutate(delta = .pred_dem - .pred_rep, win = if_else(delta > 0, 1, 0), win = sum(win)/1000, win = scales::label_percent(accuracy = 1)(win), state = paste(state, win)) %>% ungroup() %>% mutate(state = fct_reorder(state, delta)) %>% group_by(state) %>% tidybayes::median_qi(delta, .width = c(0.5, 0.8, 0.95)) %>% ggplot(aes(x = state, y = delta, ymin = .lower, ymax = .upper)) + ggdist::geom_pointinterval() + coord_flip() + theme_rieke()

  # summarise with upper/lower bounds
  pivot_longer(starts_with(".pred"),
               names_to = "party",
               values_to = ".pred_pct") %>%
  group_by(rowid, party) %>%
  summarise(.pred = quantile(.pred_pct, probs = 0.5),
            .pred_lower = quantile(.pred_pct, probs = 0.1),
            .pred_upper = quantile(.pred_pct, probs = 0.9)) %>%
  ungroup() %>%

  # join together with actual results
  mutate(party = case_match(party,
                            ".pred_dem" ~ "democratic",
                            ".pred_rep" ~ "republican",
                            ".pred_oth" ~ "other")) %>%
  left_join(state_results %>%
              select(state, year, democratic, republican, other) %>%
              rowid_to_column() %>%
              pivot_longer(c(democratic, republican, other),
                           names_to = "party",
                           values_to = "result")) %>%

  # nest(data = -state) %>%
  # slice_sample(n = 12) %>%
  # unnest(data) %>%

  # filter(state %in% c("Arizona",
  #                     "Florida",
  #                     "Georgia",
  #                     "Iowa",
  #                     "Michigan",
  #                     "Nevada",
  #                     "New Hampshire",
  #                     "North Carolina",
  #                     "Ohio",
  #                     "Pennsylvania",
  #                     "Texas",
  #                     "Wisconsin")) %>%
  # ggplot(aes(x = year,
  #            color = party)) +
  # geom_ribbon(aes(ymin = .pred_lower,
  #                 ymax = .pred_upper,
  #                 fill = party,
  #                 color = NULL),
  #             alpha = 0.15) +
  # geom_line(aes(y = .pred)) +
  # geom_point(aes(y = result)) +
  # scale_y_percent() +
  # NatParksPalettes::scale_fill_natparks_d("Triglav") +
  # NatParksPalettes::scale_color_natparks_d("Triglav") +
  # facet_wrap(~state) +
  # theme_rieke()

  # plot!
  filter(year > 1972) %>%
  filter(party != "other") %>%
  mutate(span = .pred_upper - .pred_lower) %>%
  ggplot(aes(x = span,
             fill = party)) +
  geom_histogram(position = "identity",
                 alpha = 0.5) +
  NatParksPalettes::scale_fill_natparks_d("Triglav") +
  facet_wrap(~year) +
  theme_rieke()

  ggplot(aes(x = result,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper,
             color = party)) +
  geom_pointrange(alpha = 0.25) +
  geom_abline(linetype = "dashed",
              linewidth = 0.5) +
  scale_xy_percent() +
  NatParksPalettes::scale_color_natparks_d("Triglav") +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior simulation of state results in presidential elections**",
       subtitle = "Based on incumbency, partisanship, and the presence of a third party on the ticket",
       x = "Result",
       y = "Prior prediction",
       caption = "Based on 1,000 prior simulations") +

  facet_wrap(~year)

ggquicksave("dev/05-state-priors/prior_model.png")

