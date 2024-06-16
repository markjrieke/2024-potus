plot_prob <- function(state = "National",
                      col_b,
                      col_t,
                      ...,
                      col_hline = "#363a3c",
                      linewidth = 0.3,
                      size_prob_pt = 2.5) {

  # setup p_win/p_lose based on state
  if (state == "National") {

    prob_data <-
      read_csv("out/polls/win_pres.csv") %>%
      left_join(read_csv("out/polls/tie_pres.csv")) %>%
      mutate(p_lose = 1 - p_win - p_tie)

  } else {

    # internal renaming for filtering
    state_int <- state

    prob_data <-
      read_csv("out/polls/win_state.csv") %>%
      filter(state == state_int) %>%
      mutate(p_lose = 1 - p_win)

  }

  # setup df for plotting
  prob_data <-
    prob_data %>%
    mutate(p_win_pt = if_else(run_date == max(run_date), p_win, NA),
           p_lose_pt = if_else(run_date == max(run_date), p_lose, NA),
           p_win_pos = case_when(p_win_pt > 225/538 & p_win_pt < 269/538 ~ 225/538,
                                 p_win_pt < 1 - 225/538 & p_win_pt >= 269/538 ~ 1 - 225/538,
                                 p_win_pt > 0.91 ~ 0.91,
                                 .default = p_win_pt),
           p_win_text = case_when(p_win_pt > 0.99 ~ ">99%",
                                  p_win_pt < 0.01 ~ "<1%",
                                  .default = scales::label_percent(accuracy = 1)(p_win_pt)),
           p_lose_text = case_when(p_lose_pt > 0.99 ~ ">99%",
                                   p_lose_pt < 0.01 ~ "<1%",
                                   .default = scales::label_percent(accuracy = 1)(p_lose_pt)),
           current_date = if_else(run_date == max(run_date), run_date, NA),
           date_pt = label_date_ordinal(current_date),
           p_color = p_win/(p_win + p_lose),
           tooltip = glue::glue("{label_date_ordinal(run_date)}<br>",
                                "Biden: <span style='float:right;'>",
                                "{scales::label_percent(accuracy = 1)(p_win)}",
                                "</span><br>",
                                "Trump: <span style='float:right;'>",
                                "{scales::label_percent(accuracy = 1)(p_lose)}",
                                "</span>"))

  # extract p_win/lose_pos for current day
  p_win_pos <-
    prob_data %>%
    filter(run_date == max(run_date)) %>%
    pull(p_win_pos)

  p_lose_pos <- 1 - p_win_pos

  prob_plot <-
    prob_data %>%
    ggplot(aes(x = run_date)) +

    # plot dressing
    geom_hline(yintercept = c(0, 0.5),
               color = col_hline,
               linewidth = linewidth) +
    annotate_end_date(ymin = 0,
                      ymax = 1) +
    annotate_current_date(ymin = 0,
                          ymax = 1) +

    # median estimates
    geom_underline(aes(y = p_lose),
                   color = col_t) +
    geom_underline(aes(y = p_win),
                   color = col_b) +

    # current median estimates
    geom_point(aes(y = p_lose_pt),
               color = col_t,
               size = size_prob_pt) +
    geom_point(aes(y = p_win_pt),
               color = col_b,
               size = size_prob_pt) +

    # text for current estimates
    geom_current_text(aes(x = current_date,
                          label = paste("Trump", p_lose_text, sep = "\n")),
                      y = p_lose_pos,
                      color = col_t) +
    geom_current_text(aes(x = current_date,
                          label = paste("Biden", p_win_text, sep = "\n")),
                      y = p_win_pos,
                      color = col_b) +

    # tooltip for historical data
    geom_tooltip(aes(y = 0.5,
                     tooltip = tooltip),
                 height = 1) +

    # plot dressing
    theme_2024(ymin = 0,
               ymax = 1,
               breaks = c(0, 0.25, 0.5, 0.75, 1),
               labels = c("0%", "25%", "50%", "75%", "100%"))

  # render interactive plot
  render_interactive(prob_plot)

}

