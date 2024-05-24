#' Run the pvi model
#'
#' @description
#' The pvi model is a basic linear model that estimates the partisan lean in
#' each state for each election cycle given estimated partisan lean using Cook's
#' Partisan Voter Index (cpvi) methodology.
#'
#' Partisan lean is a measure of how each state votes relative to the country.
#' For example, if the democrat wins 58% of the vote in California and 53% of
#' the vote nationally, California's partisan lean for that cycle is D+5%.
#'
#' cpvi is an apriori estimate of a state's partisan lean and is given as the
#' weighted average of the state's partisan lean in the two previous election
#' cycles: `cpvi = 0.75 * pvi_1m + 0.25 * pvi_2m`.
#'
#' The pvi model augments the cpvi estimate by including an estimate of
#' variance. The outputs are saved as the `pvi_summary`.
#'
#' The pvi model doesn't need to be run every day. This function will only run
#' the model if it has yet to run or if any of the following "trigger files"
#' have been modified since the last run:
#'
#' * data/static/statewide_results.csv
#' * data/static/abramovitz.csv
#' * stan/pvi.stan
#'
#' @param run_date the model run date. This has no bearing on the output of the
#' model, but is noted in the logs.
run_pvi_model <- function(run_date = Sys.Date()) {

  # evaluate processing time
  start_ts <- Sys.time()

  # files that will trigger a rerun of the model
  triggers <- c(
    "data/static/statewide_results.csv",
    "data/static/abramovitz.csv",
    "stan/pvi.stan"
  )

  # only rerun if necessary
  rerun <- out_of_date("out/pvi/pvi_summary.csv", triggers)
  if (!rerun) {
    cli::cli_alert_info("`pvi` up to date. Skipping model run.")
    return(invisible(NULL))
  }

  # national results
  nat_2pv <-
    read_csv("data/static/abramovitz.csv") %>%
    select(year, dem, rep) %>%
    transmute(year = year,
              nat_2pv = dem/(dem + rep))

  # import and wrangle pvi
  pvi <-
    read_csv("data/static/statewide_results.csv") %>%
    filter(year >= 1972) %>%
    select(year, state, democratic, republican) %>%
    mutate(state_2pv = democratic / (democratic + republican)) %>%
    select(year, state, state_2pv) %>%
    left_join(nat_2pv) %>%
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
           C_hat)

  # current pvi estimate for passing to 2024
  C_hat <-
    pvi %>%
    filter(year == 2020) %>%
    arrange(state)

  # compile model to exe
  pvi_model <-
    cmdstan_model(
      "stan/pvi.stan",
      dir = "exe/"
    )

  # pass data to stan
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
      S = nrow(C_hat),
      C_hat = C_hat$C_hat
    )

  # fit!
  pvi_fit <-
    pvi_model$sample(
      data = stan_data,
      seed = 2024,
      iter_warmup = 1000,
      iter_sampling = 1000,
      chains = 4,
      parallel_chains = 4
    )

  # extract prior for state pvi
  pvi_summary <-
    pvi_fit$summary("P_hat") %>%
    bind_cols(pvi %>% filter(year == max(year)) %>% arrange(state)) %>%
    select(state,
           pvi_mu = mean,
           pvi_sd = sd)

  # overwrite results if exist
  pvi_summary %>%
    write_csv("out/pvi/pvi_summary.csv")

  # diagnostics
  diagnostics <-
    pvi_fit %>%
    diagnostic_summary()

  # evaluate processing time
  end_ts <- Sys.time()

  # generate model log
  model_log <-
    tibble(
      model_name = "pvi",
      model_version = file.info("stan/pvi.stan")$mtime,
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
