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

# hmm something funky is going on & need to redo this model
# either the model is wrong or the previously done prior check was wrong

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

prior_fit_03$draws("prob", format = "df") %>%
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

####################### WORK TO DO BRUH START HERE #############################

# prior model ------------------------------------------------------------------

sims <- 1000
sigma <- 0.15

even <- 0.475
inc_bonus <- 0.025
wt <- 2

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
          beta_3d1 = rnorm(sims, 0.15, sigma),
          beta_3d2 = rnorm(sims, -0.15, sigma),
          beta_3r1 = rnorm(sims, -0.15, sigma),
          beta_3r2 = rnorm(sims, 0.15, sigma),

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
         third_party_3 = -(third_party_1 + third_party_2)) %>%

  # join to data & select relevant cols for data
  nest(data = everything()) %>%
  bind_cols(state_results %>%
              mutate(across(starts_with("pvi"),
                            ~rethinking::standardize(.x))),
            parameters = .) %>%
  rowid_to_column() %>%
  select(rowid,
         iid,
         starts_with("pvi"),
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
                            4 ~ irp_3)) %>%
  select(-starts_with("idr"),
         -starts_with("idp"),
         -starts_with("irr"),
         -starts_with("irp")) %>%

  # apply linear model
  mutate(phi_1 = inc_1 + pvi_3d*beta_3d1 + pvi_3r*beta_3r1 + third_party_flag*third_party_1,
         phi_2 = inc_2 + pvi_3d*beta_3d2 + pvi_3r*beta_3r2 + third_party_flag*third_party_2,
         phi_3 = inc_3 + pvi_3d*beta_3d3 + pvi_3r*beta_3r3 + third_party_flag*third_party_3) %>%
  select(rowid, starts_with("phi")) %>%

  # extract individual party probabilities
  mutate(prob = pmap(list(phi_1, phi_2, phi_3), ~softmax(c(..1, ..2, ..3))),
         .pred_dem = map_dbl(prob, ~.x[1]),
         .pred_rep = map_dbl(prob, ~.x[2]),
         .pred_oth = map_dbl(prob, ~.x[3])) %>%
  select(rowid, starts_with(".pred")) %>%

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
              select(democratic, republican, other) %>%
              rowid_to_column() %>%
              pivot_longer(c(democratic, republican, other),
                           names_to = "party",
                           values_to = "result")) %>%

  # plot!
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
       caption = "Based on 1,000 prior simulations")


