#' Append model logs with metadata
#'
#' @description
#' First checks if the log file already exists. If it does, `append_logs()` will
#' append the current log with `data`. If it doesn't, `append_logs()` creates
#' the log.
#'
#' @param data a tibble containing the metadata for the current model run
append_logs <- function(data) {

  if (!file.exists("out/model_log.csv")) {

    data %>%
      write_csv("out/model_log.csv")

  } else {

    read_csv("out/model_log.csv") %>%
      bind_rows(data) %>%
      write_csv("out/model_log.csv")

  }

}

#' Append daily model outputs with new estimates
#'
#' @description
#' First check if a model output file already exists. If it doesn't,
#' `append_daily_estimate()` will create the output file. If it does,
#' `append_daily_estimate()` will remove any existing data for `run_date` and
#' append the output file with `data`.
#'
#' @param data tibble containing the model output for the current run. **Must
#' include a `run_date` column!**
#' @param file path to a csv file containing the model outputs from prior runs
#' @param run_date the date for the current model run
append_daily_estimate <- function(data, file, run_date) {

  # internal variable renaming
  int_run_date <- run_date

  # force run_date column existence
  data <-
    data %>%
    mutate(run_date = run_date)

  if (!file.exists(file)) {

    cli::cli_alert_info(glue:::glue("{file} doesn't exist. Initializing file."))
    data %>%
      write_csv(file)

  } else {

    # read in current file
    current_file <-
      read_csv(file)

    # warn if overwriting existing dates
    if (int_run_date %in% current_file$run_date) {

      cli::cli_alert_warning(glue::glue("{file} already contains data for {run_date}. This will be overwritten!"))

    }

    # filter out the current run date & append
    current_file %>%
      filter(run_date != int_run_date) %>%
      bind_rows(data) %>%
      write_csv(file)

  }

}

#' Extract days that a model has not been run
#'
#' @description
#' Generate a vector of dates for which there is no model output in `file`. If
#' `file` doesn't exist, returns the sequence for all days between 5/1/24 and
#' `Sys.Date()`. Otherwise, returns a vector with days not found in `file`.
#'
#' @param file path to a csv file containing model outputs from prior runs
missing_days <- function(file) {

  days <-
    seq.Date(
      from = mdy("5/1/24"),
      to = Sys.Date(),
      by = "day"
    )

  if (file.exists(file)) {

    # pull in the days that have been run
    existing_days <-
      read_csv(file) %>%
      distinct(run_date) %>%
      pull(run_date)

    # remove any previously run days
    days <- days[!days %in% existing_days]

  }

  return(days)

}

#' Check whether an output file is out-of-date
#'
#' @description
#' Compare the last-modified timestamps of an output file and any trigger files.
#' When any trigger file has a more recent timestamp than the output file or the
#' output file doesn't exist, `out_of_date()` returns `TRUE`. Otherwise, returns
#' `FALSE`.
#'
#' @param output file path of the output file
#' @param triggers file path(s) of the trigger file(s)
out_of_date <- function(output, triggers) {

  if (file.exists(output)) {

    output_time <- file.info(output)$mtime
    trigger_times <- file.info(triggers)$mtime
    out_of_date <- any(trigger_times > output_time)

  } else {

    out_of_date <- TRUE

  }

  return(out_of_date)

}

