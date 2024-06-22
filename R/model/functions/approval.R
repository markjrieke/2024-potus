#' Run the approval model
#'
#' @description
#' The approval model is a state-space time series model estimating Biden's
#' election day net approval. The model is fit to FiveThirtyEight's net approval
#' topline estimates.
#'
#' A prior over the election day net approval estimate is imported from the
#' approval-prior model. Early in the election cycle, changes in Biden's day-to-
#' day approval will have little effect on the election day estimate. Closer to
#' election day, the estimate will narrow to Biden's daily estimate. Daily
#' estimates of Biden's net approval rating on election day are stored under
#' `e_day_approval_current`.
#'
#' The approval model is updated daily and thus is not dependent on "trigger
#' files." Minor changes to the following files, however, warrant a note and
#' version update. Major methodological changes may warrant an explicit callout
#' in the UI or a full rerun for all days since 5/1/24.
#'
#' * out/approval-prior/delta_prior.csv
#' * stan/approval.stan
#'
#' @param run_date the model run_date
run_approval_model <- function(run_date) {

  # evaluate processing time
  start_ts <- Sys.time()

  # number of pre-election days to evaluate approval over
  D <- as.integer(mdy("11/5/2024") - mdy("5/1/2024")) + 1

  # import current approval trendlines
  approval_current <-
    fetch_approval(run_date) %>%
    arrange(date) %>%
    rowid_to_column("did") %>%
    rename(Y = net)

  # read in priors from approval-prior model
  delta_prior <-
    read_csv("out/approval-prior/delta_prior.csv")

  # compile model to exe
  approval_model <-
    cmdstan_model(
      "stan/approval.stan",
      dir = "exe/"
    )

  # pass to stan
  stan_data <-
    list(
      N = nrow(approval_current),
      D = D,
      did = approval_current$did,
      Y = approval_current$Y,
      theta0_mu = 0,
      theta0_sigma = 0.5,
      sigma_s_alpha = 2,
      sigma_s_beta = 32,
      sigma_o_alpha = 2,
      sigma_o_beta = 32,
      thetaD_mu = approval_current$Y[1] + delta_prior$mean,
      thetaD_sigma = delta_prior$sd,
      sigma_o_lim = 0.005,
      prior_check = 0
    )

  # fit !
  approval_fit <-
    approval_model$sample(
      data = stan_data,
      seed = 2024,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8,
      init = 0.01
    )

  # extract prior for current approval
  e_day_approval_current <-
    approval_fit$summary("thetaD")

  # write results to log
  e_day_approval_current %>%
    append_daily_estimate(
      "out/approval/e_day_approval_current.csv",
      run_date
    )

  # diagnostics
  diagnostics <-
    approval_fit %>%
    diagnostic_summary()

  # evaluate processing time
  end_ts <- Sys.time()

  # generate model log
  model_log <-
    tibble(
      model_name = "approval",
      model_version = file.info("stan/approval.stan")$mtime,
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

#' Import most up-to-date Biden net approval topline
#'
#' @description
#' Import approval data from FiveThirtyEight between 5/1/24 and the set run_date
#'
#' @param run_date model run date. Set as a max for the approval data.
fetch_approval <- function(run_date) {

  read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv") %>%
    mutate(net = (approve_estimate - disapprove_estimate)/100) %>%
    select(date = end_date,
           net) %>%
    filter(date >= mdy("5/1/24"),
           date <= run_date)

}
