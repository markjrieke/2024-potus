#' TODO: DOCUMENT
headline_text <- function(state = "National",
                          ...,
                          branch = "dev",
                          font = "Playfair Display",
                          size = 36) {

  if (state == "National") {

    # import current win probability
    p_win <-
      read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
      filter(run_date == max(run_date))

    # set current date
    current_date <-
      p_win %>%
      pull(run_date) %>%
      label_date_ordinal()

    # extract from tibble
    p_win <-
      p_win %>%
      pull(p_win)

    # import probability of an electoral tie
    p_tie <-
      read_csv(find_document("out/polls/tie_pres.csv", branch = branch)) %>%
      filter(run_date == max(run_date)) %>%
      pull(p_tie)

    # estimate trump's probability of winning
    p_lose <- 1 - p_win - p_tie

    # rating used to set header text
    p_rating <- p_win/(p_win + p_lose)

  } else {

    # internal renaming for filtering
    state_int <- state

    # import current win probability
    p_win <-
      read_csv(find_document("out/polls/win_state.csv", branch = branch)) %>%
      filter(state == state_int,
             run_date == max(run_date))

    # set current date
    current_date <-
      p_win %>%
      pull(run_date) %>%
      label_date_ordinal()

    # extract from tibble
    p_rating <-
      p_win %>%
      pull(p_win)

  }

  # convert rating probability to text
  rating <-
    case_when(p_rating > 0.99 | p_rating < 0.01 ~ "all but guaranteed",
              p_rating > 0.85 | p_rating < 0.15 ~ "very likely",
              p_rating > 0.65 | p_rating < 0.35 ~ "likely",
              .default = "uncertain")

  # set leader/trailer text
  leader <- if (p_rating > 0.5) "Joe Biden" else "Donald Trump"
  trailer <- if (leader == "Joe Biden") "Donald Trump" else "Joe Biden"

  # Date-prefix is different for the final e-day forecast
  prefix_text <-
    if_else(
      current_date == "November 5th",
      glue::glue("The final pre-election forecast is that"),
      glue::glue("As of {current_date}, the model thinks that")
    )

  # set state to display based on rating/state
  state_text <-
    case_when(
      rating == "uncertain" & state == "National" ~ "the presidency",
      rating == "uncertain" & state != "National" ~ glue::glue("in {state}"),
      state == "National" ~ "the electoral college",
      .default = state
    )

  # rating text changes if outcome is uncertain
  rating_text <-
    if_else(
      rating == "uncertain",
      glue::glue("it's **unclear whether Joe Biden or Donald Trump will win** {state_text}."),
      glue::glue("**{leader} is {rating} to beat {trailer}** in {state_text}.")
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

#' TODO: DOCUMENT
#' TODO: UPDATE LINKS
margin_text <- function() {

  national_link <- "**[National Forecast](https://en.wikipedia.org/wiki/United_States)**"
  methods_link <- "[How this works](https://www.thedatadiary.net/posts/2022-09-16-forecast-launch/)"
  state_links <- generate_links()

  out <-
    glue::glue(
      national_link,
      methods_link,
      "<br>",
      "**Competitive states**",
      state_links$competitive,
      "<br>",
      "**All states**",
      state_links$all,
      .sep = "\n\n"
    )

  return(out)

}

#' TODO: DOCUMENT
candidate_summary <- function(candidate,
                              color,
                              ...,
                              branch = "dev") {

  # determine probability of winning for each candidate
  biden_win <-
    read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(p_win)

  p_tie <-
    read_csv(find_document("out/polls/tie_pres.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    pull(p_tie)

  trump_win <- 1 - biden_win - p_tie

  # pull in current electoral college range
  current_evs <-
    read_csv(find_document("out/polls/evs.csv", branch = branch)) %>%
    filter(run_date == max(run_date),
           .width == 0.95)

  biden_evs_lower <- current_evs %>% pull(.lower)
  biden_evs_upper <- current_evs %>% pull(.upper)

  # set text based on candidate
  if (candidate == "Joe Biden") {

    p_win <- biden_win
    evs_lower <- biden_evs_lower
    evs_upper <- biden_evs_upper
    win_text <- "winning re-election"

  } else {

    p_win <- trump_win
    evs_lower <- 538 - biden_evs_upper
    evs_upper <- 538 - biden_evs_lower
    win_text <- "re-taking the white house"

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
      "He's projected to win between **{color_text(evs_lower, color)}** and **{color_text(evs_upper, color)}** electoral college votes.<br></p>",
      "</span>",
      .sep = "\n"
    )

  return(out)

}

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



