#' Generate a formatted summary of the forecast for the specified state
#'
#' @description
#' Generates the headline summary of the forecast for the specified state. When
#' `state = "National"`, generates a summary of the forecast for the overall
#' winner of the presidency.
#'
#' @param state state to generate the summary for
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
#' @param font font used in the headline
#' @param size headline font size
headline_text <- function(state = "National",
                          ...,
                          branch = "dev",
                          font = "Playfair Display",
                          size = 36) {

  if (state == "National") {

    # import current win probability
    p_win <-
      read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
      filter(run_date == max(run_date)) %>%
      pull(p_win)

    # import probability of an electoral tie
    p_tie <-
      read_csv(find_document("out/polls/tie_pres.csv", branch = branch)) %>%
      filter(run_date == max(run_date)) %>%
      pull(p_tie)

    # estimate trump's probability of winning
    p_lose <- 1 - p_win - p_tie

  } else {

    # internal renaming for filtering
    state_int <- state

    # import current win probability
    p_win <-
      read_csv(find_document("out/polls/win_state.csv", branch = branch)) %>%
      filter(state == state_int,
             run_date == max(run_date))

    # extract from tibble
    p_win <-
      p_win %>%
      pull(p_win)

    # estimate trump's probability of winning
    p_lose <- 1 - p_win

  }

  # set current date
  current_date <-
    read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(run_date) %>%
    label_date_ordinal()

  # set leader/trailer text
  leader <- if (p_win > p_lose) "Kamala Harris" else "Donald Trump"
  trailer <- if (leader == "Kamala Harris") "Donald Trump" else "Kamala Harris"

  # set display probability
  p_rating <- if (p_win > p_lose) p_win else p_lose

  # Date-prefix is different for the final e-day forecast
  prefix_text <-
    if_else(
      current_date == "November 5th",
      glue::glue("The final pre-election forecast gives"),
      glue::glue("As of {current_date}, the forecast gives")
    )

  # modify state name for D.C.
  state_name <-
    if_else(
      state == "District of Columbia",
      "the District of Columbia",
      state
    )

  # set state to display based on rating/state
  state_text <-
    if_else(
      state_name == "National",
      "the electoral college",
      state_name
    )

  # set percentage label if greater than 99%
  percentage_text <-
    if_else(
      p_rating > 0.99,
      ">99%",
      scales::label_percent(accuracy = 1)(p_rating)
    )

  # set rating text
  rating_text <-
    glue::glue(
      "**{leader} a {percentage_text} chance of beating {trailer}** in {state_text}."
    )

  # apply header formatting
  out <-
    glue::glue(
      "<span style='font-family:{font};font-size:{size}px;'>",
      "{prefix_text} {rating_text}",
      "</span>"
    )

  return(out)

}

#' Generate the margin text that links to individual state pages
#'
#' @description
#' Applies formatting to `generate_links()` for use in the site margin.
#'
#' @param branch github branch to extract data from. Defaults to `"dev"`.
margin_text <- function(branch = "dev") {

  national_link <- "**[National Forecast](National.qmd)**"
  methods_link <- "[How this works](../posts/2024-07-04-forecast-methodology/index.qmd)"
  state_links <- generate_links(branch = branch)

  out <-
    glue::glue(
      national_link,
      paste0(methods_link, "\n\n"),
      "**Competitive states**",
      paste0(state_links$competitive, "\n\n"),
      "**All states**",
      state_links$all,
      .sep = "<br>"
    )

  return(out)

}

#' Generate a summary blurb for each candidate on the national page
#'
#' @description
#' Generates a summary blurb for the specified candidate, including their
#' current probability of winning and the 95% credible interval for the
#' posterior-predictive electoral college votes.
#'
#' @param candidate candidate name (i.e., "Harris" or "Trump")
#' @param color color used to highlight probability of winning and range of
#'              electoral college votes
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
candidate_summary <- function(candidate,
                              color,
                              ...,
                              branch = "dev") {

  # determine probability of winning for each candidate
  harris_win <-
    read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(p_win)

  p_tie <-
    read_csv(find_document("out/polls/tie_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(p_tie)

  trump_win <- 1 - harris_win - p_tie

  # pull in current electoral college range
  current_evs <-
    read_csv(find_document("out/polls/evs.csv", branch = branch)) %>%
    filter(run_date == max(run_date),
           .width == 0.95)

  harris_evs_lower <- current_evs %>% pull(.lower)
  harris_evs_upper <- current_evs %>% pull(.upper)

  # set text based on candidate
  if (candidate == "Kamala Harris") {

    p_win <- harris_win
    evs_lower <- harris_evs_lower
    evs_upper <- harris_evs_upper
    win_text <- "being elected America's next president"
    pronoun <- "She"

  } else {

    p_win <- trump_win
    evs_lower <- 538 - harris_evs_upper
    evs_upper <- 538 - harris_evs_lower
    win_text <- "re-taking the white house"
    pronoun <- "He"

  }

  # format strings
  p_win <- scales::label_percent(accuracy = 1)(p_win)
  evs_lower <- scales::label_comma(accuracy = 1)(evs_lower)
  evs_upper <- scales::label_comma(accuracy = 1)(evs_upper)

  # generate formatted string
  out <-
    glue::glue(
      "<span style='display:flex;align-items:center;height:120px;'>",
      "<p><br>**{color_text(candidate, color)}** currently has a **{color_text(p_win, color)}** chance of {win_text}.",
      "{pronoun}'s projected to win between **{color_text(evs_lower, color)}** and **{color_text(evs_upper, color)}** electoral college votes.<br></p>",
      "</span>",
      .sep = "\n"
    )

  return(out)

}

#' Generate formatted text outlining the probability of an electoral tie
#'
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
tie_text <- function(..., branch = "dev") {

  # import current probability of a tie in the electoral college
  p_tie <-
    read_csv(find_document("out/polls/tie_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(p_tie)

  # format text
  if (p_tie < 0.01) {
    tie_text <- "less than 1%"
  } else {
    tie_text <- scales::label_percent(accuracy = 1)(p_tie)
  }

  # generate formatted string
  out <-
    glue::glue(
      "<span style='color:gray;font-size:12px'>",
      "There is a {tie_text} chance of a tie in the electoral college.",
      "</span>"
    )

  return(out)

}



