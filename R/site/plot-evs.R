#' Generate an interactive plot of the electoral college forecast
#'
#' @param col_h color for plotting Harris' estimates/credible intervals
#' @param col_t color for plotting Trump's estimates/credible intervals
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
#' @param alpha_ribbon alpha parameter passed to `ggplot2::geom_ribbon()`
#' @param col_hline color passed to `ggplot2::geom_hline()` used to mark 270
#'                  votes and the y-intercept
#' @param linewidth linewidth passed to `ggplot2::geom_hline()`
#' @param size_evs_pt size of the current day median estimate passed to
#'                    `ggplot2::geom_point()`
plot_evs <- function(col_h,
                     col_t,
                     ...,
                     branch = "dev",
                     alpha_ribbon = 0.125,
                     col_hline = "#363a3c",
                     linewidth = 0.3,
                     size_evs_pt = 2.5) {


  # prep dataframe for plotting
  ev_data <-
    read_csv(find_document("out/polls/evs.csv", branch = branch)) %>%
    pivot_wider(names_from = .width,
                values_from = c(.lower, .upper)) %>%
    mutate(evs_pt = if_else(run_date == max(run_date), evs, NA),
           evs_pos = case_when(evs_pt > 225 & evs_pt < 269 ~ 225,
                               evs_pt < 538 - 225 & evs_pt >= 269 ~ 538 - 225,
                               .default = evs_pt),
           current_date = if_else(run_date == max(run_date), run_date, NA),
           date_pt = label_date_ordinal(current_date)) %>%
    mutate(tooltip = glue::glue("{label_date_ordinal(run_date)}<br>",
                                "Harris: <span style='float:right;'>",
                                "{scales::label_comma(accuracy = 1)(evs)}",
                                "</span><br>",
                                "Trump: <span style='float:right;'>",
                                "{scales::label_comma(accuracy = 1)(538 - evs)}",
                                "</span>"))

  # extract evs_pos for current day
  evs_pos <-
    ev_data %>%
    filter(run_date == max(run_date)) %>%
    pull(evs_pos)

  ev_plot <-
    ev_data %>%
    ggplot(aes(x = run_date)) +
    geom_hline(yintercept = c(0, 270),
               color = col_hline,
               linewidth = linewidth) +

    # trump ec credible interval
    geom_ribbon(aes(ymin = 538 - .upper_0.95,
                    ymax = 538 - .lower_0.95),
                fill = col_t,
                alpha = alpha_ribbon) +
    geom_ribbon(aes(ymin = 538 - .upper_0.66,
                    ymax = 538 - .lower_0.66),
                fill = col_t,
                alpha = alpha_ribbon) +

    # harris ec credible interval
    geom_ribbon(aes(ymin = .lower_0.95,
                    ymax = .upper_0.95),
                fill = col_h,
                alpha = alpha_ribbon) +
    geom_ribbon(aes(ymin = .lower_0.66,
                    ymax = .upper_0.66),
                fill = col_h,
                alpha = alpha_ribbon) +

    # plot dressing
    annotate_end_date(ymin = 0,
                      ymax = 538) +
    annotate_current_date(ymin = 0,
                          ymax = 538) +

    # median estimates
    geom_underline(aes(y = 538 - evs),
                   color = col_t) +
    geom_underline(aes(y = evs),
                   color = col_h) +

    # current median estimates
    geom_point(aes(y = 538 - evs_pt),
               color = col_t,
               size = size_evs_pt) +
    geom_point(aes(y = evs_pt),
               color = col_h,
               size = size_evs_pt) +

    # text for current estimates
    geom_current_text(aes(x = current_date,
                          label = paste("Trump", scales::label_comma(accuracy = 1)(538 - evs_pt),
                                        sep = "\n")),
                      y = 538 - evs_pos,
                      color = col_t) +
    geom_current_text(aes(x = current_date,
                          label = paste("Harris", scales::label_comma(accuracy = 1)(evs_pt),
                                        sep = "\n")),
                      y = evs_pos,
                      color = col_h) +

    # tooltip for historical data
    geom_tooltip(aes(y = 270,
                     tooltip = tooltip),
                 height = 538) +

    # plot dressing
    theme_2024(ymin = 0,
               ymax = 538,
               breaks = c(0, 70, 170, 270, 370, 470, 538),
               labels = c("0", "70", "170", "**270 to win**", "370", "470", "538"),
               ylims = c(0, 538))

  # render interactive plot
  render_interactive(ev_plot)

}


