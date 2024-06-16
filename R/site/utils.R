#' TODO: DOCUMENT
label_date_ordinal <- function(x) {

  m <- scales::label_date("%B")(x)
  d <- scales::label_ordinal()(day(x))

  out <- paste(m, d)
  out <- if_else(str_detect(out, "NA"), NA_character_, out)

  return(out)

}

#' TODO: DOCUMENT
generate_links <- function() {

  # currently just links to wikipedia page
  links <-
    read_csv("out/polls/win_state.csv") %>%
    filter(run_date == max(run_date)) %>%
    mutate(heat = 1 - abs(0.5 - p_win) * 2,
           competitive = if_else(p_win > 0.15 & p_win < 0.85,
                                 "competitive",
                                 "safe")) %>%
    arrange(desc(heat)) %>%
    filter(state != "National") %>%
    mutate(state = glue::glue("[{state}](https://en.wikipedia.org/wiki/{state})"))

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
