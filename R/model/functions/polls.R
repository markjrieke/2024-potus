#' Run the poll model
#'
#' @description
#' The poll model is a dynamic state-space model that uses polling results to
#' estimate the latent probability of supporting Biden over Trump in each state
#' on each day in the election cycle by way of a binomial likelihood.
#'
#' State effects are modeled hierarchically via a Gaussian Process over the
#' Euclidean distance between states in a feature space of state
#' characteristics. Similarly, the random walk in the state space makes use of
#' the hierarchical specification such that day-over-day movement in one state
#' can influence the movement a similar state even in the absence of polls.
#' Finally, polling bias in each state makes further use of the hierarchical
#' specification.
#'
#' Several other measures of bias are included as parameters in the model. The
#' model makes use of fixed effects for whether or not a poll was conducted with
#' partisan sponsorship and for the sampled population (non-partisan polls and
#' likely-voter polls are utilized as reference categories). Effects for poll
#' mode and pollster are modeled hierarchically. Finally, the model includes a
#' hierarchical parameter per poll to capture any further unmodeled bias.
#'
#' Polls are imported from FiveThirtyEight's data repository. Polls are filtered
#' and wrangled according to the following criteria:
#'
#' * The poll cannot have been conducted by one of the banned pollsters
#' * The poll must have been conducted between 5/1/2024 and 11/5/2024
#' * The question must include both Biden and Trump as possible named candidates
#' * The question cannot include hypothetical candidates
#' * In the case of multiple questions per poll, the following filters are
#'   applied in order
#'    * Filter to the question with the minimum number of candidates to more
#'      closely match the modeling assumption of a binomial likelihood
#'    * Filter to the "most preferred" voting population (LV > RV > V > A)
#'    * Filter to the poll with the largest sample size
#'    * If these filters do not result in a single question per poll, the
#'      remaining results are averaged by candidate across questions (this can
#'      happen in the case of multiple voter models being recorded per question)
#'
#' The poll model estimates the following values for election day:
#'
#' * theta: Biden's posterior predicted voteshare in each state on election day
#' * win_state: the posterior probability of Biden winning each state on
#'              election day
#' * evs: the posterior prediction for the number of electoral college votes
#'        Biden will win on election day
#' * win_pres: the posterior probability of Biden winning the presidency on
#'             election day
#' * tie_pres: the posterior probability of an electoral tie (269-269)
#'
#' The poll model is updated daily and thus is not dependent on "trigger files."
#' Minor changes to the following files, however, warrant a note and version
#' update. Major methodological changes may warrant an explicit callout in the
#' UI or a full rerun for all days since 5/1/24.
#'
#' * data/static/banned_pollsters.csv
#' * data/static/allowed_candidates.csv
#' * data/static/population_rank.csv
#' * data/features/sid.csv
#' * data/features/F_r.rds
#' * data/features/wt.rds
#' * out/priors/priors.csv
#' * stan/polls.stan
#'
#' @param run_date the model run_date
run_poll_model <- function(run_date) {

  # evaluate processing time
  start_ts <- Sys.time()

  # internal variable renaming
  int_run_date <- run_date

  # number of days in election cycle
  D <- as.integer(mdy("11/5/2024") - mdy("5/1/2024") + 1)

  # import polls from fte
  polls <-
    read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")

  # read in static data
  banned_pollsters <- read_csv("data/static/banned_pollsters.csv")
  allowed_candidates <- read_csv("data/static/allowed_candidates.csv")
  population_rank <- read_csv("data/static/population_rank.csv")

  # read in pre-computed feature data
  sid <- read_csv("data/features/sid.csv")
  F_r <- read_rds("data/features/F_r.rds")
  wt <- read_rds("data/features/wt.rds")

  # read in priors
  priors <-
    read_csv("out/priors/priors.csv") %>%
    mutate(state = str_replace(state, "[ ]+(?=[0-9])", " CD-")) %>%
    filter(run_date == int_run_date,
           !state %in% c("National", "Nebraska", "Maine"))

  # wrangle polls
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
    filter(!pollster %in% banned_pollsters$pollster) %>%

    # filter to only polls taken since may of the election year
    mutate(end_date = mdy(end_date),
           pct = pct/100) %>%
    filter(end_date >= mdy("5/1/2024"),
           end_date < mdy("11/5/2024")) %>%

    # filter based on run date
    filter(end_date <= run_date) %>%

    # only care about polls that include both biden & trump
    group_by(question_id) %>%
    mutate(biden_trump = str_detect(candidate, "Biden|Trump"),
           biden_trump = sum(biden_trump)) %>%
    ungroup() %>%
    filter(biden_trump == 2) %>%

    # remove questions with hypothetical candidates
    group_by(question_id) %>%
    mutate(allowed_candidate = candidate %in% allowed_candidates$candidate,
           allowed_candidate = min(allowed_candidate)) %>%
    ungroup() %>%
    filter(allowed_candidate != 0) %>%
    select(-allowed_candidate) %>%

    # total number of candidates and total percent responding
    # used to subset later on
    group_by(poll_id, question_id) %>%
    mutate(n_candidates = n(),
           total_pct = sum(pct)) %>%

    # select the question most closely matching the model (min candidates)
    # if multiple populations polled, select the "best rank"
    left_join(population_rank) %>%
    group_by(poll_id) %>%
    filter(n_candidates == min(n_candidates),
           rank == min(rank)) %>%
    select(-c(rank, n_candidates)) %>%
    ungroup() %>%

    # only care about the results for biden & trump
    filter(candidate %in% c("Biden", "Trump")) %>%

    # use the response with the max total percent responding
    # (distinguishes between B/T & B/T/no-response)
    group_by(poll_id) %>%
    filter(total_pct == max(total_pct)) %>%
    select(-total_pct) %>%

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

    # get into wide format
    mutate(candidate = str_to_lower(candidate),
           responses = round(pct * sample_size)) %>%
    select(-pct) %>%
    pivot_wider(names_from = candidate,
                values_from = responses) %>%
    mutate(sample_size = biden + trump) %>%
    select(-c(trump, poll_id)) %>%

    # fix missing values
    mutate(state = replace_na(state, "National"),
           mode = replace_na(mode, "Unknown"),
           candidate_sponsored = replace_na(candidate_sponsored, "None")) %>%
    rename(group = population)

  # add mapping ids by day
  polls <-
    polls %>%
    mutate(did = as.integer(mdy("11/5/2024") - end_date),
           did = as.integer(mdy("11/5/2024") - mdy("5/1/2024")) - did + 1)

  # create other mapping id tables
  pid <- polls %>% map_ids(pollster)
  gid <- polls %>% map_ids(group)
  mid <- polls %>% map_ids(mode)
  cid <- polls %>% map_ids(candidate_sponsored)

  # append with mapping ids
  polls <-
    polls %>%
    left_join(sid) %>%
    left_join(pid) %>%
    left_join(gid) %>%
    left_join(mid) %>%
    left_join(cid) %>%
    rename(K = sample_size,
           Y = biden)

  # enforce prior ordering
  priors <-
    priors %>%
    left_join(sid %>% select(sid, state)) %>%
    arrange(sid) %>%
    select(-c(sid, run_date))

  # compile model to exe
  poll_model <-
    cmdstan_model(
      "stan/polls.stan",
      dir = "exe/"
    )

  # pass to stan
  stan_data <-
    list(
      N = nrow(polls),
      D = D,
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
      g_ref = gid %>% filter(group == "lv") %>% pull(gid),
      c_ref = cid %>% filter(candidate_sponsored == "None") %>% pull(cid),
      F_r = F_r,
      wt = wt,
      K = polls$K,
      Y = polls$Y,
      beta_g_sigma = 0.02,
      beta_c_sigma = 0.015,
      sigma_n_sigma = 0.02,
      sigma_p_sigma = 0.05,
      sigma_m_sigma = 0.02,
      e_day_mu_r = priors$e_day_mu,
      e_day_mu_sigma_r = priors$e_day_sigma,
      rho_alpha = 3,
      rho_beta = 6,
      alpha_sigma = 0.05,
      phi_sigma = 0.05,
      psi_sigma = 0.05,
      omega = 600,
      electors = sid$electors,
      prior_check = 0
    )

  # fit !
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

  # post-process voteshare in each state
  poll_fit$draws(paste0("theta[", 1:max(sid$sid), ",", D, "]"),
                 format = "df") %>%
    as_tibble() %>%
    pivot_longer(starts_with("theta"),
                 names_to = "sid",
                 values_to = "theta") %>%
    mutate(sid = str_remove_all(sid, "theta\\[|,189\\]"),
           sid = as.integer(sid)) %>%
    group_by(sid) %>%
    tidybayes::median_qi(theta, .width = c(0.66, 0.95)) %>%
    left_join(sid) %>%
    select(state,
           theta,
           .lower,
           .upper,
           .width) %>%
    append_daily_estimate("out/polls/theta.csv",
                          run_date)

  # probability of winning in each state
  poll_fit$summary(paste0("win_state[", 1:max(sid$sid), ",", D, "]")) %>%
    mutate(sid = str_remove_all(variable, "win_state\\[|,189\\]"),
           sid = as.integer(sid)) %>%
    left_join(sid) %>%
    select(state,
           p_win = mean) %>%
    append_daily_estimate("out/polls/win_state.csv",
                          run_date)

  # number of electoral college votes
  poll_fit$draws(paste0("evs[", D, "]"),
                 format = "df") %>%
    as_tibble() %>%
    rename(evs = 1) %>%
    tidybayes::median_qi(evs, .width = c(0.66, 0.95)) %>%
    select(evs,
           .lower,
           .upper,
           .width) %>%
    append_daily_estimate("out/polls/evs.csv",
                          run_date)

  # probability of winning the presidency
  poll_fit$summary(paste0("win_pres[", D, "]")) %>%
    select(p_win = mean) %>%
    append_daily_estimate("out/polls/win_pres.csv",
                          run_date)

  # probability of an electoral college tie
  poll_fit$summary(paste0("tie_pres[", D, "]")) %>%
    select(p_tie = mean) %>%
    append_daily_estimate("out/polls/tie_pres.csv",
                          run_date)

  # diagnostics
  diagnostics <-
    poll_fit %>%
    diagnostic_summary()

  # evaluate processing time
  end_ts <- Sys.time()

  # generate model log
  model_log <-
    tibble(
      model_name = "polls",
      model_version = file.info("stan/polls.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = stan_data$N,
      num_divergent = diagnostics$num_divergent,
      num_max_treedepth = diagnostics$num_max_treedepth,
      run_date = run_date
    )

  # write logs
  model_log %>% append_logs()

}

#' Generate a reference table mapping a feature column to an id
#'
#' @description
#' Mapping id's are generated using the first letter of the feature column. For
#' example, supplying the `pollster` column to `map_ids()` will generate a table
#' mapping each pollster name to a `pid`.
#'
#' @param .data tibble containing features to be mapped
#' @param col feature to be mapped
#'
#' @return a tibble with one row per unique category in `col`
map_ids <- function(.data, col) {

  # get id column name
  colname <- deparse(substitute(col))
  idname <- paste0(str_sub(colname, 1, 1), "id")

  # generate mapping table
  out <-
    .data %>%
    distinct({{ col }}) %>%
    arrange({{ col }}) %>%
    rowid_to_column(idname)

  return(out)

}
