# setup ------------------------------------------------------------------------

library(tidyverse)

source("R/model/imports.R")

# import data ------------------------------------------------------------------

# economic data
gdp <- fetch_gdp()
cpi <- fetch_cpi()

# polls
polls <- read_csv("data/polls/president_2020.csv")

# blegh ------------------------------------------------------------------------

population_rank <-
  tibble(population = c("lv", "rv", "a", "v"),
         rank = 1:4)

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

  # filter to only polls taken since may of the election year
  mutate(end_date = mdy(end_date),
         pct = pct/100) %>%
  filter(end_date >= mdy("5/1/2020"),
         end_date <= mdy("11/4/2020")) %>%

  # total number of candidates and total percent responding
  # used to subset later on
  group_by(poll_id, question_id) %>%
  mutate(n_candidates = n(),
         total_pct = sum(pct)) %>%

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

  mutate(state = replace_na(state, "National"),
         candidate_sponsored = replace_na(candidate_sponsored, "None")) %>%

  count(state) %>% view()
