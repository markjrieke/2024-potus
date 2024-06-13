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

#' TODO: DOCUMENT
blurb_evs <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Estimated electoral college votes</span>",
    "Our model is updated every day and combines state and national polls with economic indicators to predict a range of outcomes.",
    "The midpoint is the estimate of the electoral college vote for each party on election day.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_prob <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Presidential probabilities</span>",
    "Each day, the model simulates thousands of plausible election results.",
    "Each candidate's probability of winning is the proportion of simulations that they've won.",
    "This is an additional sentence so that the temp blurb takes up the desired amount of space.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_map <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Chance of winning each state</span>",
    "Our model combines the national prediction with polls and political-economic factors at the state level.",
    "We take into account that states that are similar are likely to move with each other; if Donald Trump wins Minnesota,",
    "he will probably win Wisconsin too.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_vote <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Forecasted voteshare on each day</span>",
    "The model first averages the polls, weighting them by their sample sizes and correcting them for tendencies to overestimate support for one party.",
    "It then combines this average with our forecast based on non-polling data, pulling vote shares on each day slightly towards the final election-day projection.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
footer <- function() {

  glue::glue(
    "<span style='color:gray;font-size:12px'>",
    "Sources: Ballotpedia; Cook Political Report; The Economist; Federal Reserve Bank of St. Louis; FiveThirtyEight; Urban Stats; 270towin.com",
    "<br>",
    "<br>",
    "Forecast by Mark Rieke",
    "<br>",
    "Copyediting by [D Rieke](https://x.com/drekrek)",
    "</span>",
    .sep = "\n"
  )

}






