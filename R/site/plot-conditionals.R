#' Generate a plot showing how each candidate's probability of winning improves
#' conditional on winning in competitive states
#'
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
plot_conditionals <- function(..., branch = "dev") {

  # import conditional probabilities
  conditional_probs <-
    read_csv(find_document("out/polls/conditional_probabilities.csv", branch = branch))

  # filter to just the competitive states (p < 0.85)
  competitive_probs <-
    generate_links()$competitive %>%
    str_split("<br>") %>%
    pluck(1) %>%
    as_tibble() %>%
    separate(value, c("state", "link"), "\\(") %>%
    select(-link) %>%
    mutate(state = str_remove_all(state, "\\[|\\]")) %>%
    left_join(conditional_probs)

  # set baseline win probs based on the selected winner
  win_probs <-
    read_csv(find_document("out/polls/win_pres.csv", branch = branch)) %>%
    left_join(read_csv(find_document("out/polls/tie_pres.csv", branch = branch))) %>%
    filter(run_date == max(run_date)) %>%
    transmute(harris_win = p_win,
              trump_win = 1 - p_win - p_tie)

  # generate individual candidate plots
  plot_harris <-
    plot_candidate_conditional(
      competitive_probs,
      "Harris",
      win_probs
    )

  plot_trump <-
    plot_candidate_conditional(
      competitive_probs,
      "Trump",
      win_probs
    )

  # join together
  plot_joint <- plot_harris + plot_trump

  return(plot_joint)

}

#' Internal function for generating one of the candidate conditional plots in
#' `plot_conditionals()`
#'
#' @param competitive_probs tibble of competitive states and their conditional
#'                          probabilities per candidate
#' @param candidate the candidate to generate the plot for
#' @param win_probs the baseline win probabilities for each candidate
#' @param ... unused
#' @param pal color palette used to accent the plot
plot_candidate_conditional <- function(competitive_probs,
                                       candidate,
                                       win_probs,
                                       ...,
                                       pal = c("#3579ac", "#7cb0d7", "#d3e5f2",
                                               "#f2f2f2",
                                               "#f2d5d5", "#d78080", "#b13737")) {

  # set plot variables based on selected candidate
  if (candidate == "Harris") {

    p_win <- win_probs$harris_win

    competitive_probs <-
      competitive_probs %>%
      mutate(arrow_col = state_win_ec_win)

    arrow_col <- pal[1]

    possessive <- "Harris'"
    pronoun <- "she"

  } else {

    p_win <- win_probs$trump_win

    competitive_probs <-
      competitive_probs %>%
      mutate(arrow_col = 1 - state_lose_ec_win - state_lose_ec_tie)

    arrow_col <- pal[7]

    possessive <- "Trump's"
    pronoun <- "he"

  }

  # plot!
  conditional_plot <-
    competitive_probs %>%
    mutate(state = fct_reorder(state, arrow_col)) %>%
    ggplot(aes(y = state)) +

    # add plot dressing elements
    geom_hline(yintercept = 0.4,
               color = "#363a3c",
               linewidth = 0.9) +
    geom_segment(data = tibble(x = c(0, 0.25, 0.5, 0.75, 1)) %>%
                   mutate(xend = x,
                          y = 0.6,
                          yend = nrow(competitive_probs) + 0.4),
                 mapping = aes(x = x,
                               y = y,
                               xend = xend,
                               yend = yend),
                 color = "gray80",
                 linewidth = 0.25) +
    geom_segment(data = tibble(y = seq(from = 1.5, to = nrow(competitive_probs) + 0.5)) %>%
                   mutate(yend = y,
                          x = -0.05,
                          xend = 1.05),
                 mapping = aes(x = x,
                               y = y,
                               xend = xend,
                               yend = yend),
                 color = "white",
                 linewidth = 6) +
    geom_segment(data = tibble(y = seq(from = 1.5, to = nrow(competitive_probs) + 0.5)) %>%
                   mutate(yend = y,
                          x = 0,
                          xend = 1),
                 mapping = aes(x = x,
                               y = y,
                               xend = xend,
                               yend = yend),
                 color = "gray80",
                 linewidth = 0.25) +

    # conditional probabilities
    geom_segment(aes(xend = arrow_col),
                 x = p_win,
                 arrow = arrow(angle = 20,
                               length = unit(0.15, "inches")),
                 linewidth = 0.8,
                 color = arrow_col) +
    geom_point(x = p_win,
               fill = "white",
               shape = 21,
               size = 3) +

    # theming
    scale_y_discrete() +
    scale_x_percent() +
    theme_rieke() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks.x = element_line(),
          axis.text.y = ggtext::element_markdown(halign = 1)) +
    labs(title = "",
         subtitle = glue::glue("How **{color_text(possessive, arrow_col)}** ",
                               "chances of winning the presidency<br>improve if {pronoun} ",
                               "**{color_text('wins', arrow_col)}** in competitive states:"),
         x = NULL,
         y = NULL) +
    expand_limits(x = c(0, 1)) +
    coord_cartesian(xlim = c(0, NA))

  return(conditional_plot)

}

