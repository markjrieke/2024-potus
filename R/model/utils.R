#' Append model logs with metadata
#'
#' @description
#' First checks if the log file already exists. If it does, `append_logs()` will
#' append the current log with `data`. If it doesn't, `append_logs()` creates
#' the log.
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

