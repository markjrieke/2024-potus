#' Run the approval prior model
#'
#' @description
#' The approval prior model is a state-space time series model estimating the
#' election day approval for incumbent candidates in election cycles from 1948-
#' 2020. `run_approval_prior_model()` generates two important outputs:
#'
#' * `delta_prior`: a prior for the expected amount of drift in presidential
#'    approval from the beginning of the election cycle until election day.
#' * `e_day_approval_historical`: a prior for the election day approval for the
#'    incumbent candidate in each cycle.
#'
#' The approval prior model doesn't need to be run every day. This function will
#' only run the model if it has yet to run or if any of the following "trigger
#' files" have been modified since the last run:
#'
#' * data/approval/historical_approval.csv
#' * data/static/abramowitz.csv
#' * stan/approval-prior.stan
#'
#' @param run_date the model run date. This has no bearing on the output of the
#' model, but is noted in the logs.
run_approval_prior_model <- function(run_date = Sys.Date()) {

  # evaluate processing time
  start_ts <- Sys.time()

  # files that will trigger a rerun of the model
  triggers <- c(
    "data/static/abramowitz.csv",
    "data/approval/historical_approval.csv",
    "stan/approval-prior.stan"
  )

  # only rerun if necessary
  rerun <- out_of_date("out/approval-prior/delta_prior.csv", triggers)
  if (!rerun) {
    cli::cli_alert_info("`approval-prior` up to date. Skipping model run.")
    return(invisible(NULL))
  }

  # number of pre-election days to evaluate approval over
  delta <- as.integer(mdy("11/5/2024") - mdy("5/1/2024")) + 1

  # import election day relative to term start
  max_day <-
    read_csv("data/static/abramowitz.csv") %>%
    select(year,
           max_day = approval_day)

  # wrangle approval data
  approval_historical <-
    read_csv("data/approval/historical_approval.csv") %>%
    mutate(year = case_when(president == "truman" & day <= 1299 ~ 1948,
                            president == "truman" & day <= 2762 ~ 1952,
                            president == "eisenhower" & day <= 1385 ~ 1956,
                            president == "eisenhower" & day <= 2848 ~ 1960,
                            president == "johnson" & day <= 346 ~ 1964,
                            president == "johnson" & day <= 1809 ~ 1968,
                            president == "nixon" & day <= 1386 ~ 1972,
                            president == "ford" & day <= 815 ~ 1976,
                            president == "carter" & day <= 1383 ~ 1980,
                            president == "reagan" & day <= 1385 ~ 1984,
                            president == "reagan" & day <= 2848 ~ 1988,
                            president == "hw_bush" & day <= 1382 ~ 1992,
                            president == "clinton" & day <= 1384 ~ 1996,
                            president == "clinton" & day <= 2847 ~ 2000,
                            president == "w_bush" & day <= 1381 ~ 2004,
                            president == "w_bush" & day <= 2844 ~ 2008,
                            president == "obama" & day <= 1385 ~ 2012,
                            president == "obama" & day <= 2848 ~ 2016,
                            president == "trump" & day <= 1382 ~ 2020)) %>%
    drop_na() %>%
    left_join(max_day) %>%
    mutate(min_day = max_day - delta + 1) %>%
    filter(day <= max_day,
           day >= min_day) %>%
    mutate(day = day - min_day + 1) %>%
    arrange(year, day) %>%
    select(-ends_with("_day"))

  # prep data for passing to stan
  approval_prior <-
    approval_historical %>%
    distinct(year) %>%
    rowid_to_column("cid") %>%
    right_join(approval_historical) %>%
    mutate(net = net/100) %>%
    relocate(cid, .after = net) %>%
    rename(did = day,
           Y = net)

  # compile model to exe
  approval_prior_model <-
    cmdstan_model(
      "stan/approval-prior.stan",
      dir = "exe/"
    )

  # format data for stan
  stan_data <-
    list(
      N = nrow(approval_prior),
      C = max(approval_prior$cid),
      D = delta,
      cid = approval_prior$cid,
      did = approval_prior$did,
      Y = approval_prior$Y,
      theta0_mu = 0,
      theta0_sigma = 0.5,
      sigma_s_alpha = 2,
      sigma_s_beta = 32,
      sigma_o_alpha = 2,
      sigma_o_beta = 32,
      sigma_o_lim = 0.005,
      prior_check = 0
    )

  # fit!
  approval_prior_fit <-
    approval_prior_model$sample(
      data = stan_data,
      seed = 2024,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8,
      init = 0.01
    )

  # extract prior for current presidential approval forecast
  delta_prior <-
    approval_prior_fit$summary("delta")

  # extract prior for e_day historical approval
  e_day_approval_historical <-
    approval_prior_fit$summary("thetaD")

  # post-process
  e_day_approval_historical <-
    e_day_approval_historical %>%
    mutate(cid = parse_number(variable)) %>%
    left_join(approval_prior %>% distinct(cid, year)) %>%
    select(year,
           A_mu = mean,
           A_sigma = sd)

  # overwrite results if exist
  delta_prior %>%
    write_csv("out/approval-prior/delta_prior.csv")

  e_day_approval_historical %>%
    write_csv("out/approval-prior/e_day_approval_historical.csv")

  # diagnostics
  diagnostics <-
    approval_prior_fit %>%
    diagnostic_summary()

  # evaluate processing time
  end_ts <- Sys.time()

  # generate model log
  model_log <-
    tibble(
      model_name = "approval-prior",
      model_version = file.info("stan/approval-prior.stan")$mtime,
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
