#' Run the poll model
#'
#' @description
#' The poll model is a dynamic state-space model that uses polling results to
#' estimate the latent probability of supporting the democratic candidate over
#' Trump in each state on each day in the election cycle by way of a binomial
#' likelihood.
#'
#' On July 21st, Joe Biden suspended his campaign for re-election and Kamala
#' Harris announced her intention to run as the democratic candidate. The model
#' estimates support for both candidates based on polls from 5/1 onwards. Harris
#' is modeled as a hypothetical candidate in polls with a start date prior to
#' July 22nd. Polls of Biden that conclude after July 21st are discarded.
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
#' partisan sponsorship, the sampled population, and whether or not Harris is
#' considered as a hypothetical candidate at the time the poll was conducted.
#' Non-partisan polls, likely-voter polls, and non-hypothetical polls are
#' utilized as reference categories for the fixed effects. Effects for poll mode
#' and pollster are modeled hierarchically. Finally, the model includes a
#' hierarchical parameter per poll to capture any further unmodeled bias.
#'
#' Polls are imported from FiveThirtyEight's data repository. Polls are filtered
#' and wrangled according to the following criteria:
#'
#' * The poll cannot have been conducted by one of the banned pollsters
#' * The poll must have been conducted between 5/1/2024 and 11/5/2024
#' * The question must include either Biden or Harris and Trump as possible
#'   named candidates
#' * With the exception of Harris prior to 7/22, the question cannot include
#'   hypothetical candidates
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
#' * theta: Harris' posterior predicted voteshare in each state on election day
#' * win_state: the posterior probability of Harris winning each state on
#'              election day
#' * evs: the posterior prediction for the number of electoral college votes
#'        Harris will win on election day
#' * win_pres: the posterior probability of Harris winning the presidency on
#'             election day
#' * tie_pres: the posterior probability of an electoral tie (269-269)
#' * conditional_probabilities: the probability of a win or tie in the electoral
#'                              college conditional on a win or tie in each
#'                              state
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
#' * data/features/F_s.rds
#' * data/features/wt.rds
#' * out/priors/priors.csv
#' * stan/polls.stan
#' * stan/functions.stan
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

  # write polls out
  polls %>%
    write_csv("out/polls/polls_raw.csv")

  # read in static data
  banned_pollsters <- read_csv("data/static/banned_pollsters.csv")
  allowed_candidates <- read_csv("data/static/allowed_candidates.csv")
  population_rank <- read_csv("data/static/population_rank.csv")

  # read in pre-computed feature data
  sid <- read_csv("data/features/sid.csv")
  F_r <- read_rds("data/features/F_r.rds")
  F_s <- read_rds("data/features/F_s.rds")
  wt <- read_rds("data/features/wt.rds")

  # read in priors
  priors <-
    read_csv("out/priors/priors.csv") %>%
    mutate(state = str_replace(state, "[ ]+(?=[0-9])", " CD-")) %>%
    filter(run_date == int_run_date,
           !state %in% c("National", "Nebraska", "Maine"))

  # modify int_run_date based on run_date
  if (run_date < mdy("5/3/24")) {

    prior_check <- 1
    int_run_date <- mdy("5/3/24")

  } else {

    prior_check <- 0

  }

  # wrangle polls
  polls <-
    polls %>%

    # select relevant columns for modeling
    select(poll_id,
           question_id,
           state,
           sample_size,
           pollster,
           start_date,
           end_date,
           created_at,
           population,
           mode = methodology,
           candidate_sponsored = partisan,
           candidate = answer,
           pct) %>%

    # remove banned pollsters
    filter(!pollster %in% banned_pollsters$pollster) %>%

    # filter to only polls taken since may of the election year
    mutate(across(ends_with("_date"), mdy),
           pct = pct/100) %>%
    filter(end_date >= mdy("5/1/2024"),
           end_date < mdy("11/5/2024")) %>%

    # filter based on run date
    mutate(created_at = as_date(mdy_hm(created_at))) %>%
    filter(created_at <= int_run_date) %>%

    # only care about polls that include biden & trump or harris & trump
    group_by(question_id) %>%
    mutate(inclusion = str_detect(candidate, "Biden|Harris|Trump"),
           inclusion = sum(inclusion)) %>%
    ungroup() %>%
    filter(inclusion == 2) %>%
    select(-inclusion) %>%

    # categorize which comparison the poll is making
    group_by(question_id) %>%
    mutate(dem_candidate = if_else(str_detect(candidate, "Biden"), 1, 0),
           dem_candidate = max(dem_candidate),
           dem_candidate = if_else(dem_candidate == 1, "Biden", "Harris")) %>%
    ungroup() %>%

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
    group_by(poll_id, dem_candidate) %>%
    filter(n_candidates == min(n_candidates),
           rank == min(rank)) %>%
    select(-rank) %>%
    ungroup() %>%

    # add flag if pre/post biden dropout announcement
    mutate(post_announcement = if_else(start_date >= mdy("7/22/24"), TRUE, FALSE)) %>%

    # apply filter at the poll_id level
    # (some pollsters poll B/T vs B/T/K in separate polls, rather than in
    #  separate questions on the same poll)
    group_by(state,
             sample_size,
             pollster,
             end_date,
             population,
             mode,
             candidate_sponsored,
             post_announcement,
             dem_candidate) %>%
    filter(n_candidates == min(n_candidates)) %>%
    select(-n_candidates) %>%
    ungroup() %>%

    # only care about the results for biden, harris, & trump
    filter(candidate %in% c("Biden", "Harris", "Trump")) %>%

    # use the response with the max total percent responding
    # (distinguishes between B/T & B/T/no-response)
    group_by(poll_id, dem_candidate) %>%
    filter(total_pct == max(total_pct)) %>%
    select(-total_pct) %>%

    # remove fully duplicate responses
    group_by(poll_id, candidate, dem_candidate) %>%
    distinct(pct, .keep_all = TRUE) %>%

    # use the max sample size in the case of multiple matches
    group_by(poll_id, dem_candidate) %>%
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
             post_announcement,
             candidate,
             dem_candidate) %>%
    summarise(pct = mean(pct)) %>%
    ungroup() %>%

    # get into wide format
    mutate(candidate = str_to_lower(candidate),
           responses = round(pct * sample_size)) %>%
    select(-pct) %>%
    bind_rows(tibble(candidate = "harris")) %>%
    pivot_wider(names_from = candidate,
                values_from = responses) %>%
    filter(!is.na(poll_id)) %>%
    mutate(dem = if_else(is.na(biden), harris, biden),
           sample_size = dem + trump) %>%
    select(-c(biden, harris, trump)) %>%

    # fix missing values
    mutate(state = replace_na(state, "National"),
           mode = replace_na(mode, "Unknown"),
           candidate_sponsored = replace_na(candidate_sponsored, "None")) %>%
    rename(group = population) %>%

    # only keep the most recent poll from tracking polls
    group_by(state,
             pollster,
             group,
             mode,
             candidate_sponsored,
             dem_candidate) %>%
    arrange(end_date) %>%
    mutate(diff = as.integer(lead(end_date) - end_date),
           diff = replace_na(diff, 100),
           final = if_else(diff > 1, 1, NA)) %>%
    filter(!is.na(final)) %>%
    ungroup() %>%
    select(-c(diff, final)) %>%

    # drop any late-arrive biden polls
    mutate(late_biden = dem_candidate == "Biden" & post_announcement) %>%
    filter(!late_biden) %>%
    select(-late_biden)

  # add mapping ids by day
  polls <-
    polls %>%
    mutate(did = as.integer(mdy("11/5/2024") - end_date),
           did = as.integer(mdy("11/5/2024") - mdy("5/1/2024")) - did + 1)

  # create other mapping id tables
  pid <- polls %>% map_ids(pollster)
  mid <- polls %>% map_ids(mode)

  # fixed mapping id tables
  gid <- population_rank %>% select(gid = rank, group = population)
  cid <- tibble(cid = 1:4, candidate_sponsored = c("DEM", "REP", "IND", "None"))
  hid <- tibble(hid = 1:2, dem_candidate = c("Harris", "Biden"))

  # append with mapping ids
  polls <-
    polls %>%
    left_join(sid) %>%
    left_join(pid) %>%
    left_join(gid) %>%
    left_join(mid) %>%
    left_join(cid) %>%
    left_join(hid) %>%
    mutate(hyp = if_else(!post_announcement & dem_candidate == "Harris", 1, 0)) %>%
    rename(K = sample_size,
           Y = dem)

  # write formatted polls out
  polls %>%
    write_csv("out/polls/polls_out.csv")

  # enforce prior ordering
  priors <-
    priors %>%
    left_join(sid %>% select(sid, state)) %>%
    arrange(sid, desc(candidate)) %>%
    select(-c(sid, run_date))

  # convert alpha priors to wide matrix format
  alpha_mu_r <- priors %>% format_alpha_prior(e_day_mu)
  alpha_sigma_r <- priors %>% format_alpha_prior(e_day_sigma)

  # set omega
  omega <- set_omega(run_date)

  # check whether or not functions.stan has been updated since last run
  if (out_of_date("exe/polls", "stan/functions.stan")) {
    force_recompile <- TRUE
  } else {
    force_recompile <- FALSE
  }

  # compile model to exe
  poll_model <-
    cmdstan_model(
      "stan/polls.stan",
      dir = "exe/",
      force_recompile = force_recompile
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
      H = 2,
      did = polls$did,
      sid = polls$sid,
      gid = polls$gid,
      mid = polls$mid,
      cid = polls$cid,
      pid = polls$pid,
      hid = polls$hid,
      g_ref = gid %>% filter(group == "lv") %>% pull(gid),
      c_ref = cid %>% filter(candidate_sponsored == "None") %>% pull(cid),
      h_ref = hid %>% filter(dem_candidate == "Harris") %>% pull(hid),
      hyp = polls$hyp,
      F_r = F_r,
      wt = wt,
      K = polls$K,
      Y = polls$Y,
      beta_g_sigma = 0.02,
      beta_c_sigma = 0.015,
      beta_hyp_sigma = 0.25,
      sigma_n_sigma = 0.02,
      sigma_p_sigma = 0.05,
      sigma_m_sigma = 0.02,
      alpha_mu_r = alpha_mu_r,
      alpha_sigma_r = alpha_sigma_r,
      rho_alpha = 3,
      rho_beta = 6,
      alpha_sigma = 0.05,
      phi_sigma = 0.05,
      psi_sigma = 0.05,
      omega = omega,
      electors = sid$electors,
      F_s = F_s,
      prior_check = prior_check
    )

  # fit !
  poll_fit <-
    poll_model$sample(
      data = stan_data,
      seed = 2024,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8,
      init = 0.01,
      step_size = 0.002,
      adapt_delta = 0.95
    )

  # post-process voteshare in each state
  poll_fit$draws("theta",
                 format = "df") %>%
    as_tibble() %>%
    pivot_longer(starts_with("theta"),
                 names_to = "sid",
                 values_to = "theta") %>%
    mutate(sid = parse_number(sid)) %>%
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
  poll_fit$summary("win_state") %>%
    mutate(sid = parse_number(variable)) %>%
    left_join(sid) %>%
    select(state,
           p_win = mean) %>%
    append_daily_estimate("out/polls/win_state.csv",
                          run_date)

  # number of electoral college votes won in each draw
  evs_won <-
    poll_fit$draws("evs",
                   format = "df") %>%
    as_tibble()

  # number of electoral college votes
  evs_won %>%
    tidybayes::median_qi(evs, .width = c(0.66, 0.95)) %>%
    select(evs,
           .lower,
           .upper,
           .width) %>%
    append_daily_estimate("out/polls/evs.csv",
                          run_date)

  # probability of winning the presidency
  poll_fit$summary("win_pres") %>%
    select(p_win = mean) %>%
    append_daily_estimate("out/polls/win_pres.csv",
                          run_date)

  # probability of an electoral college tie
  poll_fit$summary("tie_pres") %>%
    select(p_tie = mean) %>%
    append_daily_estimate("out/polls/tie_pres.csv",
                          run_date)

  # draws in which each candidate wins in each state
  state_wins <-

    # convert to long tibble
    poll_fit$draws("win_state",
                   format = "df") %>%
    as_tibble() %>%
    pivot_longer(starts_with("win"),
                 names_to = "variable",
                 values_to = "win") %>%
    filter(win > 0) %>%
    mutate(sid = parse_number(variable)) %>%
    left_join(sid) %>%
    select(state,
           draw = .draw) %>%
    nest(data = -state) %>%
    arrange(state) %>%

    # outcomes conditional on a biden win
    mutate(evs = list(evs_won),
           state_win_evs = pmap(list(data, evs), ~filter(..2, .draw %in% ..1$draw)),
           win_draws = map_dbl(state_win_evs, nrow),
           state_win_ec_win = map_dbl(state_win_evs, ~nrow(filter(.x, evs >= 270))),
           state_win_ec_win = state_win_ec_win/win_draws,
           state_win_ec_tie = map_dbl(state_win_evs, ~nrow(filter(.x, evs == 269))),
           state_win_ec_tie = state_win_ec_tie/win_draws) %>%
    select(-state_win_evs) %>%

    # outcomes conditional on a trump win
    mutate(state_lose_evs = pmap(list(data, evs), ~filter(..2, !.draw %in% ..1$draw)),
           lose_draws = map_dbl(state_lose_evs, nrow),
           state_lose_ec_win = map_dbl(state_lose_evs, ~nrow(filter(.x, evs >= 270))),
           state_lose_ec_win = state_lose_ec_win/lose_draws,
           state_lose_ec_tie = map_dbl(state_lose_evs, ~nrow(filter(.x, evs == 270))),
           state_lose_ec_tie = state_lose_ec_tie/lose_draws) %>%

    # write results
    select(state,
           win_draws,
           state_win_ec_win,
           state_win_ec_tie,
           lose_draws,
           state_lose_ec_win,
           state_lose_ec_tie) %>%
    full_join(sid %>% select(state)) %>%
    arrange(state) %>%
    write_csv("out/polls/conditional_probabilities.csv")

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

#' Generate the out-of-sample noise parameter, omega
#'
#' @description
#' Outputs the out-of-sample noise parameter, omega, based on the run date
#'
#' @param run_date model run date
#' @param e_day_omega the out-of-sample noise parameter on election day
#' @param s_day_omega the out-of-sample noise parameter on 5/1/24
set_omega <- function(run_date,
                      e_day_omega = 600,
                      s_day_omega = 100) {

  # find current day as integer
  D <- as.integer(mdy("11/5/24") - mdy("5/1/24")) + 1
  d <- as.integer(run_date - mdy("5/1/24")) + 1

  # estimate linear transform between s_day and e_day omegas on the outcome scale
  inputs <- 0.5 * c(s_day_omega, e_day_omega)
  y <- qbeta(0.975, inputs, inputs)
  x <- c(1, D)
  m <- (y[2] - y[1])/(x[2] - x[1])
  b <- y[1] - m * x[1]

  # linear transform output for current day
  q <- logit(m * d + b)

  # internal function for finding omega that generates q
  find_omega <- function(omega, q, p) {
    qnorm(p, 0, omega) - q
  }

  # optimize
  omega <- uniroot(find_omega, interval = c(0, 1), q = q, p = 0.975)$root

  return(omega)

}

#' Convert alpha priors from a long tibble to a wide matrix
#'
#' @description
#' For correct functionality, requires that the prior data supplied to `data`
#' is filtered to a single run date and arranged by state id & descending
#' candidate.
#'
#' @param data a long tibble containing priors for each state on election day
#' @param col column to be pivoted
format_alpha_prior <- function(data, col) {

  data %>%
    select(candidate, state, {{ col }}) %>%
    pivot_wider(names_from = state,
                values_from = {{ col }}) %>%
    select(-candidate) %>%
    as.matrix()

}



