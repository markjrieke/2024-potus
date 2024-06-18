# functions --------------------------------------------------------------------

#' Transform a beta-distributed variable to a normal distribution via a copula
#'
#' @param data tibble of state-level features
#' @param feature feature column to conduct the transformation over
#'
#' @return the same tibble supplied to `data` where the `feature` column is now
#' normally distributed
copula_transform <- function(data, feature) {

  # beta distributed observations
  observed <- data %>% pull({{ feature }})

  # estimate and extract parameters
  fit <-
    gamlss::gamlss(
      observed ~ 1,
      family = gamlss.dist::BEo()
    )

  alpha <- fitted(fit, "mu")[1]
  beta <- fitted(fit, "sigma")[1]

  # convert to normal space
  uniform_space <- pbeta(observed, alpha, beta)
  normal_space <- qnorm(uniform_space)

  # replace in the original data
  out <-
    data %>%
    mutate("{{feature}}" := normal_space)

  return(out)

}

# generate feature matrix ------------------------------------------------------

# import state data
urban_stats <-
  read_csv("data/static/urban_stats.csv")

# import electoral data
electors <-
  read_csv("data/static/electors.csv")

# generate feature tibble
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

# construct feature matrix as the euclidean distance in the feature space
F_r <- matrix(0, nrow = nrow(feature_matrix), ncol = nrow(feature_matrix))
for (r in 1:nrow(F_r)) {
  for (c in 1:ncol(F_r)) {
    F_r[r,c] <- (feature_matrix[r,] - feature_matrix[c,])^2 |> sum() |> sqrt()
  }
}

# scale for consistent prior allocation
F_r <- F_r/max(F_r)

# create aggregate weights
wt <-
  features %>%

  # aggregate states weighted by population within the aggegate
  transmute(state = state,
            population_mm = population_mm,
            wt_nat = 1,
            wt_ne = if_else(str_detect(state, "Nebraska"), 1, 0),
            wt_me = if_else(str_detect(state, "Maine"), 1, 0)) %>%
  mutate(across(starts_with("wt"), ~.x * population_mm),
         across(starts_with("wt"), ~.x/sum(.x))) %>%

  # convert to a A x R matrix
  select(starts_with("wt")) %>%
  as.matrix() %>%
  t()

# create id mapping table
sid <-
  features %>%
  select(state) %>%
  arrange(state) %>%
  bind_rows(tibble(state = c("National", "Nebraska", "Maine"))) %>%
  left_join(electors) %>%
  mutate(electors = replace_na(electors, 0)) %>%
  rowid_to_column("sid")

# generate aggregate-inc feature matrix ----------------------------------------

aggregate_features <-
  features %>%

  # update aggregate state features
  filter(str_detect(state, "Nebraska|Maine")) %>%
  mutate(state = str_remove(state, " CD-[:digit:]"),
         pw_density = exp(log_pw_density)) %>%
  rename(pop = population_mm) %>%
  group_by(state) %>%

  # aggregate features are a population-weighted sum of subgroups
  summarise(population_mm = sum(pop),
            log_pw_density = log(sum(pop * pw_density)/sum(pop)),
            white = sum(pop * white)/sum(pop),
            hispanic = sum(pop * hispanic)/sum(pop),
            black = sum(pop * black)/sum(pop),
            asian = sum(pop * asian)/sum(pop),
            citizen_by_birth = sum(pop * citizen_by_birth)/sum(pop),
            ed_index = sum(pop * ed_index)/sum(pop),
            inc_index = sum(pop * inc_index)/sum(pop)) %>%

  # append back to main feature set & filter out subgroups
  bind_rows(features) %>%
  filter(!str_detect(state, "CD-"))

# convert to matrix
aggregate_feature_matrix <-
  aggregate_features %>%
  select(log_pw_density:inc_index) %>%
  as.matrix()

# construct feature matrix as the euclidean distance in the feature space
F_a <- matrix(0, nrow = nrow(aggregate_feature_matrix), ncol = nrow(aggregate_feature_matrix))
for (r in 1:nrow(F_a)) {
  for (c in 1:ncol(F_a)) {
    F_a[r,c] <- (aggregate_feature_matrix[r,] - aggregate_feature_matrix[c,])^2 |> sum() |> sqrt()
  }
}

# invert and scale
F_a <- 1 - (F_a/max(F_a))

# apply state names to rows/cols
rownames(F_a) <- aggregate_features$state
colnames(F_a) <- aggregate_features$state

# write out --------------------------------------------------------------------

# write out matrices as .rds files
F_r %>% write_rds("data/features/F_r.rds")
F_a %>% write_rds("data/features/F_a.rds")
wt %>% write_rds("data/features/wt.rds")

# write out mapping table as .csv
sid %>%
  write_csv("data/features/sid.csv")


