#' Represent a date as a string with ordinal formatting
#'
#' @description
#' Converts a date object to a string representation of the date with ordinal
#' formatting for the day and excluding the year (i.e.,
#' `label_date_ordinal(lubridate::mdy("5/1/24"))` returns `"May 1st"`).
#'
#' @param x date to be converted
label_date_ordinal <- function(x) {

  m <- scales::label_date("%B")(x)
  d <- scales::label_ordinal()(day(x))

  out <- paste(m, d)
  out <- if_else(str_detect(out, "NA"), NA_character_, out)

  return(out)

}

#' Reference a document on the specified branch in github, rather than locally
#'
#' @param document path to document, including document suffix
#' @param branch github branch to extract data from. Defaults to `"dev"`.
find_document <- function(document,
                          branch = "dev") {

  glue::glue(
    "https://raw.githubusercontent.com/markjrieke/2024-potus/{branch}/{document}"
  )

}


#' Generate a set of links to competitive states and a set to all states
#'
#' @description
#' Generates two set of links used to navigate to state forecast pages:
#'
#' * Competitive states: states where the leading candidate has < 85% chance of
#'                       winning. Links are ordered by "heat", a measure of how
#'                       close the is to a pure 50-50 tossup.
#' * All states: all state pages, ordered alphabetically.
#'
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
generate_links <- function(..., branch = "dev") {

  # currently just links to wikipedia page
  links <-
    read_csv(find_document("out/polls/win_state.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    mutate(heat = 1 - abs(0.5 - p_win) * 2,
           competitive = if_else(p_win > 0.15 & p_win < 0.85,
                                 "competitive",
                                 "safe")) %>%
    arrange(desc(heat)) %>%
    filter(state != "National") %>%
    mutate(state = glue::glue("[{state}]({state}.qmd)"))

  # competitive states (< 0.99 prob)
  competitive <-
    links %>%
    filter(competitive == "competitive") %>%
    pull(state) %>%
    str_c(collapse = "<br>") %>%
    glue::as_glue()

  # all states (alphabetical)
  all <-
    links %>%
    arrange(state) %>%
    pull(state) %>%
    str_c(collapse = "<br>") %>%
    glue::as_glue()

  out <-
    list(
      competitive = competitive,
      all = all
    )

  return(out)

}
