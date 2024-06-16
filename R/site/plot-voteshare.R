#' TODO: DOCUMENT
plot_vote <- function(state,
                      col_b,
                      col_t,
                      ...,
                      alpha_ribbon = 0.125,
                      col_hline = "#363a3c",
                      linewidth = 0.3,
                      size_theta_pt = 2.5) {

  # internal renaming for filtering
  state_int <- state

  # generate plot data
  voteshare <-
    read_csv("out/polls/theta.csv") %>%
    filter(state == state_int) %>%
    pivot_wider(names_from = .width,
                values_from = c(.lower, .upper)) %>%
    mutate(theta_pt = if_else(run_date == max(run_date), theta, NA),
           current_date = if_else(run_date == max(run_date), run_date, NA),
           date_pt = label_date_ordinal(current_date),
           tooltip = glue::glue("{label_date_ordinal(run_date)}<br>Biden: ",
                                "<span style='float:right;'>",
                                "{scales::label_percent(accuracy = 0.1)(theta)}",
                                "</span><br>Trump: ",
                                "<span style='float:right;'>",
                                "{scales::label_percent(accuracy = 0.1)(1 - theta)}",
                                "</span>"))

  # plot elements that are independent of plot limits
  vote_plot <-
    voteshare %>%
    ggplot(aes(x = run_date)) +
    geom_hline(yintercept = 0.5,
               color = col_hline,
               linewidth = linewidth) +

    # trump voteshare credible interval
    geom_ribbon(aes(ymin = 1 - .upper_0.95,
                    ymax = 1 - .lower_0.95),
                fill = col_t,
                alpha = alpha_ribbon) +
    geom_ribbon(aes(ymin = 1 - .upper_0.66,
                    ymax = 1 - .lower_0.66),
                fill = col_t,
                alpha = alpha_ribbon) +

    # biden voteshare credible interval
    geom_ribbon(aes(ymin = .lower_0.95,
                    ymax = .upper_0.95),
                fill = col_b,
                alpha = alpha_ribbon) +
    geom_ribbon(aes(ymin = .lower_0.66,
                    ymax = .upper_0.66),
                fill = col_b,
                alpha = alpha_ribbon)

  # extract y limits so I can set the x-axis & segments correctly
  y_limits <- ggplot_build(vote_plot)$layout$panel_params[[1]]$y.range

  # set breaks & labels explicitly so that new ones aren't introduced
  breaks <- layer_scales(vote_plot)$y$break_positions()
  labels <- scales::label_percent(accuracy = 1)(breaks)

  # position of the voteshare estimates on the graph
  theta_pt <-
    voteshare %>%
    filter(run_date == max(run_date)) %>%
    pull(theta_pt)

  # set lower/upper bounds of theta_pos
  pos_lower <- 225/538 * (y_limits[2] - y_limits[1]) + y_limits[1]
  pos_upper <- 1 - pos_lower

  # set position for shadowtext
  theta_pos <-
    case_when(theta_pt > pos_lower & theta_pt < 0.5 ~ pos_lower,
              theta_pt < pos_upper & theta_pt >= 0.5 ~ pos_upper,
              .default = theta_pt)

  # carry on my wayward son
  vote_plot <-
    vote_plot +

    # plot dressing
    annotate_end_date(ymin = y_limits[1],
                      ymax = y_limits[2]) +
    annotate_current_date(ymin = y_limits[1],
                          ymax = y_limits[2]) +
    geom_hline(yintercept = y_limits[1],
               color = col_hline,
               linewidth = linewidth) +

    # median estimates
    geom_underline(aes(y = 1 - theta),
                   color = col_t) +
    geom_underline(aes(y = theta),
                   color = col_b) +

    # current median estimates
    geom_point(aes(y = 1 - theta_pt),
               color = col_t,
               size = size_theta_pt) +
    geom_point(aes(y = theta_pt),
               color = col_b,
               size = size_theta_pt) +

    # text for current estimates
    geom_current_text(aes(x = current_date,
                          label = paste("Trump", scales::label_percent(accuracy = 0.1)(1 - theta_pt),
                                        sep = "\n")),
                      y = 1 - theta_pos,
                      color = col_t) +
    geom_current_text(aes(x = current_date,
                          label = paste("Biden", scales::label_percent(accuracy = 0.1)(theta_pt),
                                        sep = "\n")),
                      y = theta_pos,
                      color = col_b) +

    # tooltip for historical data
    geom_tooltip(aes(y = 0.5,
                     tooltip = tooltip),
                 height = y_limits[2] - y_limits[1]) +

    # plot dressing
    theme_2024(ymin = y_limits[1],
               ymax = y_limits[2],
               breaks = breaks,
               labels = labels)

  # render interactive plot
  render_interactive(vote_plot)

}

