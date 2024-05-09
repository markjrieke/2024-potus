# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

source("R/model/imports.R")
source("R/model/utils.R")

# import data ------------------------------------------------------------------

# economic data
gdp <- fetch_gdp()
cpi <- fetch_cpi()

# polls
polls <- read_csv("data/polls/president_2020.csv")

# State-level data
urban_stats <- read_csv("data/static/urban_stats.csv")

# Prior model data
abramovitz <- read_csv("data/static/abramovitz.csv")

# TODO: convert to csv
population_rank <-
  tibble(population = c("lv", "rv", "v", "a"),
         rank = 1:4)

# TODO: convert to csv
allowed_candidates <-
  c("Biden", "Trump", "Jorgensen", "Hawkins")

# TODO: convert to csv (& add others [Traflagar & Center Street])
banned_pollsters <-
  c("Rasmussen", "Traflagar", "Center Street PAC")

# construct state-level feature matrix -----------------------------------------

# TODO: this can be done in data
features <-
  urban_stats %>%

  # exclude aggregate states
  filter(!state %in% c("National", "Nebraska", "Maine")) %>%

  # construct indices from features
  mutate(across(white:inc_geq_100, ~.x/100),
         ed_less_hs = 1 - ed_high_school,
         ed_high_school = ed_high_school - ed_undergrad,
         ed_undergrad = ed_undergrad - ed_grad,
         ed_index = ed_high_school + 2 * ed_undergrad + 3 * ed_grad,
         inc_index = inc_less_50 - poverty + inc_50_100 + 2 * inc_geq_100,
         log_pw_density = log(pw_density)) %>%

  # copula transform beta-distributed features
  copula_transform(white) %>%
  copula_transform(hispanic) %>%
  copula_transform(black) %>%
  copula_transform(asian) %>%
  copula_transform(citizen_by_birth) %>%

  # standardize feature columns
  select(state,
         population_mm,
         log_pw_density,
         white,
         hispanic,
         black,
         asian,
         citizen_by_birth,
         ed_index,
         inc_index) %>%
  mutate(across(log_pw_density:inc_index, standardize)) %>%
  arrange(state)

# convert to matrix
feature_matrix <-
  features %>%
  select(log_pw_density:inc_index) %>%
  as.matrix()

# Construct feature matrix as the euclidean distance in the feature space
F_r <- matrix(0, nrow = nrow(feature_matrix), ncol = nrow(feature_matrix))
for (r in 1:nrow(F_r)) {
  for (c in 1:ncol(F_r)) {
    F_r[r,c] <- (feature_matrix[r,] - feature_matrix[c,])^2 |> sum() |> sqrt()
  }
}

# Scale for consistent prior allocation
F_r <- F_r/max(F_r)

# Create aggregate weights
wt <-
  features %>%

  # aggregate states weighted by population within the aggregate
  transmute(state = state,
            population_mm = population_mm,
            wt_nat = 1,
            wt_ne = if_else(str_detect(state, "Nebraska"), 1, 0),
            wt_me = if_else(str_detect(state, "Maine"), 1, 0)) %>%
  mutate(across(starts_with("wt"), ~.x * population_mm),
         across(starts_with("wt"), ~.x/sum(.x))) %>%

  # convert to A x R matrix
  select(starts_with("wt")) %>%
  as.matrix() %>%
  t()

# create id mapping table
sid <-
  features %>%
  select(state) %>%
  bind_rows(tibble(state = c("National", "Nebraska", "Maine"))) %>%
  rowid_to_column("sid")

# wrangle polls ----------------------------------------------------------------

# wrangle polls
# TODO: this should be a function in the final flow
polls <-
  polls %>%

  # select relevant columns for modeling
  select(poll_id,
         question_id,
         state,
         sample_size,
         pollster,
         end_date,
         population,
         mode = methodology,
         candidate_sponsored = partisan,
         candidate = answer,
         pct) %>%

  # remove banned pollsters
  filter(!pollster %in% banned_pollsters) %>%

  # filter to only polls taken since may of the election year
  mutate(end_date = mdy(end_date),
         pct = pct/100) %>%
  filter(end_date >= mdy("5/1/2020"),
         end_date < mdy("11/3/2020")) %>%

  # remove questions with hypothetical candidates
  group_by(question_id) %>%
  mutate(allowed_candidate = candidate %in% allowed_candidates,
         allowed_candidate = min(allowed_candidate)) %>%
  ungroup() %>%
  filter(allowed_candidate != 0) %>%
  select(-allowed_candidate) %>%

  # total number of candidates and total percent responding
  # used to subset later on
  group_by(poll_id, question_id) %>%
  mutate(n_candidates = n(),
         total_pct = sum(pct))  %>%

  # select the question most closely matching model (min candidates)
  # if multiple populations polled, select "best rank"
  left_join(population_rank) %>%
  group_by(poll_id) %>%
  filter(n_candidates == min(n_candidates),
         rank == min(rank)) %>%
  select(-rank) %>%
  ungroup() %>%

  # only care about polls including both biden & trump
  filter(candidate %in% c("Biden", "Trump")) %>%
  group_by(poll_id, question_id) %>%
  mutate(n_candidates = n()) %>%
  filter(n_candidates == 2) %>%

  # use the response with max total percent
  # (distinguishes between B/T and B/T/no-response)
  filter(total_pct == max(total_pct)) %>%
  select(-c(total_pct, n_candidates)) %>%

  # remove fully duplicate responses
  group_by(poll_id, candidate) %>%
  distinct(pct, .keep_all = TRUE) %>%

  # use the max sample size in the case of multiple matches
  group_by(poll_id) %>%
  filter(sample_size == max(sample_size)) %>%

  # average results across turnout models if necessary
  group_by(poll_id,
           state,
           sample_size,
           pollster,
           end_date,
           population,
           mode,
           candidate_sponsored,
           candidate) %>%
  summarise(pct = mean(pct)) %>%
  ungroup() %>%
  select(-poll_id) %>%

  # get into wide format
  mutate(candidate = str_to_lower(candidate),
         responses = round(pct * sample_size)) %>%
  select(-pct) %>%
  pivot_wider(names_from = candidate,
              values_from = responses) %>%
  mutate(sample_size = biden + trump) %>%
  select(-trump) %>%

  # fix missing values
  mutate(state = replace_na(state, "National"),
         mode = replace_na(mode, "Unknown"),
         candidate_sponsored = replace_na(candidate_sponsored, "None")) %>%
  rename(group = population)

# create mapping ids
pid <- polls %>% map_ids(pollster)
gid <- polls %>% map_ids(group)
mid <- polls %>% map_ids(mode)
cid <- polls %>% map_ids(candidate_sponsored)

# prep for stan
polls <-
  polls %>%
  mutate(did = as.integer(mdy("11/3/2020") - end_date),
         did = as.integer(mdy("11/3/2020") - mdy("5/1/2020")) - did + 1) %>%
  left_join(sid) %>%
  left_join(pid) %>%
  left_join(gid) %>%
  left_join(mid) %>%
  left_join(cid) %>%
  rename(K = sample_size,
         Y = biden)

# pvi model --------------------------------------------------------------------

# TODO: extend to all years
pvi <-
  read_csv("data/static/statewide_results.csv") %>%
  filter(year >= 1972) %>%
  select(year, state, democratic, republican) %>%
  mutate(state_2pv = democratic / (democratic + republican)) %>%
  select(year, state, state_2pv) %>%
  left_join(read_csv("data/static/abramovitz.csv") %>%
              select(year, dem, rep) %>%
              transmute(year = year,
                        nat_2pv = dem/(dem + rep))) %>%
  mutate(pvi = state_2pv - nat_2pv) %>%
  select(year, state, pvi) %>%
  mutate(ym4 = year - 4,
         ym8 = year - 8) %>%
  left_join(x = .,
            y = select(.,
                       ym4 = year,
                       state,
                       pvim4 = pvi)) %>%
  left_join(x = .,
            y = select(.,
                       ym8 = year,
                       state,
                       pvim8 = pvi)) %>%
  drop_na() %>%
  mutate(cpvi = 0.75 * pvim4 + 0.25 * pvim8,
         C_hat = 0.75 * pvi + 0.25 * pvim4) %>%
  select(year,
         state,
         P = pvi,
         C = cpvi,
         C_hat) %>%
  filter(year < 2020)

stan_data <-
  list(
    N = nrow(pvi),
    P = pvi$P,
    C = pvi$C,
    alpha_mu = 0,
    alpha_sigma = 1,
    beta_mu = 0,
    beta_sigma = 1,
    sigma_sigma = 1,
    S = nrow(pvi %>% filter(year == max(year))),
    C_hat = pvi %>% filter(year == max(year)) %>% pull(C_hat)
  )

pvi_model <-
  cmdstan_model("stan/pvi_model.stan")

pvi_fit <-
  pvi_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 4,
    parallel_chains = 4
  )

pvi_fit$summary("P_hat") %>%
  bind_cols(pvi %>% filter(year == max(year))) %>%
  ggplot(aes(x = C_hat,
             y = median,
             ymin = q5,
             ymax = q95)) +
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  scale_xy_percent() +
  theme_rieke()

# prior model ------------------------------------------------------------------

# TODO: convert this to function
# TODO: only run if metadata out-of-date

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

# append national data with 2nd quarter real annualized gdp growth
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

# summarize abramovitz data
model_data <-
  abramovitz %>%
  mutate(fte_net_inc_approval = fte_net_inc_approval/100) %>%
  select(year,
         inc_party,
         I = inc_running,
         dem,
         rep,
         A = fte_net_inc_approval,
         G = real_gdp_growth) %>%
  transmute(V = if_else(inc_party == "dem", dem/(dem + rep), rep/(dem + rep)),
            A = A,
            G,
            I)

# pass to stan
stan_data <-
  list(
    N = nrow(model_data),
    V = model_data$V,
    A = model_data$A,
    G = model_data$G,
    I = model_data$I,
    A_new = (abramovitz %>% filter(year == 2020) %>% pull(fte_net_inc_approval))/100,
    G_new = abramovitz %>% filter(year == 2020) %>% pull(real_gdp_growth),
    I_new = abramovitz %>% filter(year == 2020) %>% pull(inc_running),
    sigma_hat = 0.05
  )

prior_model <-
  cmdstan_model("stan/prior_model.stan")

prior_fit <-
  prior_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 4,
    parallel_chains = 4
  )

national_prior <-
  prior_fit$draws("theta_new_pred", format = "df") %>%
  as_tibble() %>%
  transmute(theta = 1 - theta_new_pred)

priors <-
  read_csv("data/static/statewide_results.csv") %>%
  filter(year %in% c(2012, 2016)) %>%
  select(year, state, democratic, republican) %>%
  pivot_longer(c(democratic, republican),
               names_to = "party",
               values_to = "voteshare") %>%
  mutate(voteshare = voteshare/100,
         party = paste(str_sub(party, 1, 3), year - 2000, sep = "_")) %>%
  group_by(year, state) %>%
  mutate(state_2pv = voteshare/sum(voteshare)) %>%
  ungroup() %>%
  filter(str_detect(party, "dem")) %>%
  select(year, state, state_2pv) %>%
  left_join(abramovitz %>%
              mutate(nat_2pv = dem/(dem + rep)) %>%
              select(year, nat_2pv)) %>%
  mutate(pvi = state_2pv - nat_2pv,
         year = paste("pvi", year - 2000, sep = "_")) %>%
  select(year, state, pvi) %>%
  pivot_wider(names_from = year,
              values_from = pvi) %>%
  mutate(pvi = 0.75 * pvi_16 + 0.25 * pvi_12) %>%
  select(state, pvi) %>%
  filter(!state %in% c("National", "Nebraska", "Maine")) %>%
  mutate(state = str_replace(state, "[ ]+(?=[0-9])", " CD-")) %>%
  crossing(national_prior) %>%
  mutate(prior = pvi + theta,
         prior = logit(prior)) %>%
  drop_na() %>%
  group_by(state) %>%
  summarise(e_day_mu = mean(prior),
            e_day_sigma = sd(prior)) %>%
  left_join(sid) %>%
  filter(!is.na(sid)) %>%
  arrange(sid)

# model ------------------------------------------------------------------------

stan_data <-
  list(
    N = nrow(polls),
    D = as.integer(mdy("11/3/2020") - mdy("5/1/2020")) + 1,
    R = nrow(F_r),
    A = nrow(wt),
    S = max(sid$sid),
    G = max(gid$gid),
    M = max(mid$mid),
    C = max(cid$cid),
    P = max(pid$pid),
    did = polls$did,
    sid = polls$sid,
    gid = polls$gid,
    mid = polls$mid,
    cid = polls$cid,
    pid = polls$pid,
    F_r = F_r,
    wt = wt,
    K = polls$K,
    Y = polls$Y,
    beta_g_sigma = 0.05,
    beta_c_sigma = 0.05,
    sigma_n_sigma = 0.05,
    sigma_p_sigma = 0.075,
    sigma_m_sigma = 0.05,
    e_day_mu_r = priors$e_day_mu,
    e_day_sigma_r = priors$e_day_sigma,
    rho_alpha = 3,
    rho_beta = 6,
    alpha_sigma = 0.05,
    phi_sigma = 0.05,
    omega = 1000,
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
    init = 0.01
  )

results <-
  poll_fit$summary("theta")

results %>%
  mutate(variable = str_remove_all(variable, "theta\\[|]")) %>%
  separate(variable, c("sid", "day"), ",") %>%
  mutate(across(c(sid, day), as.integer)) %>%
  left_join(sid) %>% #filter(state == "Pennsylvania", day == 186)
  nest(data = -state) %>%
  slice_sample(n = 9) %>%
  unnest(data) %>%
  ggplot(aes(x = day,
             y = median,
             ymin = q5,
             ymax = q95)) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray60") +
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  scale_y_percent() +
  facet_wrap(~state) +
  theme_rieke()

pollsters <-
  poll_fit$summary("eta_p")

pollsters %>%
  mutate(variable = as.integer(parse_number(variable))) %>%
  rename(pid = variable) %>%
  left_join(pid) %>%
  left_join(polls %>% count(pid)) %>%
  filter(n >= 10) %>%
  mutate(pollster = paste0(pollster, "\n(n = ", n, ")"),
         pollster = fct_reorder(pollster, median)) %>%
  ggplot(aes(x = pollster,
             y = -median,
             ymin = -q95,
             ymax = -q5)) +
  geom_pointrange() +
  coord_flip() +
  theme_rieke()
