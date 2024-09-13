#' Generate an interactive map of the US with states colored by their forecast
#' rating
#'
#' @description
#' Generates an interactive map of the US with states colored by their forecast
#' rating. The size of each state bubble is scaled to the number of electors in
#' the state and the color color is scaled to the probability of each candidate
#' winning in the state.
#'
#' @param ... unused
#' @param pal color palette to map probability of winning
#' @param branch github branch to extract data from. Defaults to `"dev"`.
plot_map <- function(...,
                     pal = c("#3579ac", "#7cb0d7", "#d3e5f2",
                             "#f2f2f2",
                             "#f2d5d5", "#d78080", "#b13737"),
                     branch = "dev") {

  # generate clickable links
  clickables <-
    read_csv(find_document("data/static/electors.csv")) %>%
    select(state) %>%
    mutate(link = glue::glue("window.open(\"{state}.html\")"))

  # base map
  us_geo <-
    read_rds(find_document("data/tigris/tigris.rds", branch = branch))

  # set color and fill based on probability of winning
  state_ratings <-
    read_csv(find_document("out/polls/win_state.csv", branch = branch)) %>%
    filter(run_date == max(run_date)) %>%
    left_join(read_csv(find_document("data/static/electors.csv", branch = branch))) %>%
    drop_na() %>%
    mutate(rating = map_chr(p_win, ~interpolate_fill(.x, pal)),
           color = map_chr(p_win, ~interpolate_fill(.x, c(rep("white", 2),
                                                          "gray60",
                                                          "black",
                                                          "gray60",
                                                          rep("white", 2)))))

  # state vector used to relevel barchart
  state_relevel <-
    state_ratings %>%
    arrange(p_win) %>%
    pull(state)

  # plot state bubbles and tooltips
  map_points <-
    state_ratings %>%
    left_join(read_csv(find_document("data/static/state_centers.csv", branch = branch))) %>%
    left_join(clickables) %>%
    sf::st_as_sf(coords = c("long", "lat"),
                 crs = 4326) %>%
    mutate(candidate = if_else(p_win >= 0.5, "Harris", "Trump"),
           p_label = if_else(candidate == "Harris", p_win, 1 - p_win),
           p_label = if_else(p_label > 0.99, ">99%", scales::label_percent(accuracy = 1)(p_label)),
           ev_label = if_else(electors == 1, "EV", "EVs"),
           text_color = if_else(candidate == "Harris", pal[1], pal[7]),
           summary_text = glue::glue("<b>{candidate}</b> has a <b>{p_label}</b> chance of winning"),
           tooltip = glue::glue("<span style='font-family:IBM Plex Sans'><span style='text-align:left;'><b>{state}</b></span><span style='float:right;font-size:12px;'>({electors} {ev_label})</span>",
                                "<span style='font-size:14px'>{summary_text}</span></span>",
                                .sep = "<br>"),
           tooltip = glue::glue("{color_text(tooltip, text_color)}"))

  # us map!
  map_ratings <-
    us_geo %>%
    ggplot() +
    geom_sf(fill = "#fafafa",
            color = "#ededed",
            linewidth = 0.5) +
    geom_sf_interactive(data = map_points,
                        aes(size = electors,
                            fill = rating,
                            onclick = link,
                            color = color,
                            tooltip = tooltip,
                            data_id = state),
                        shape = 21) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size_continuous(range = c(4.5, 18)) +
    theme_void() +
    theme(legend.position = "none")

  # counts of non-competitive races for summary bar
  non_competitive_d <-
    state_ratings %>%
    filter(p_win > 0.85) %>%
    pull(electors) %>%
    sum()

  non_competitive_r <-
    state_ratings %>%
    filter(p_win < 0.15) %>%
    pull(electors) %>%
    sum()

  # generate summary bar
  map_bar <-
    map_points %>%
    mutate(state = fct_relevel(state, state_relevel)) %>%
    ggplot(aes(x = 0,
               y = electors,
               fill = rating)) +
    geom_bar_interactive(aes(onclick = link,
                             tooltip = tooltip,
                             data_id = state,
                             group = state),
                         position = "stack",
                         stat = "identity") +
    geom_rect(aes(ymin = non_competitive_d,
                  ymax = 538 - non_competitive_r,
                  xmin = -0.45,
                  xmax = 0.45),
              fill = NA,
              color = "black") +
    geom_segment(x = -0.45,
                 xend = 0.45,
                 y = 269,
                 yend = 269,
                 color = "black",
                 linetype = "dotted") +
    geom_text(x = 0.7,
              y = 269,
              label = "270 to win",
              family = "IBM Plex Sans",
              color = "#363a3c",
              size = 3.25) +
    geom_text(x = -0.7,
              y = non_competitive_d + (538 - non_competitive_d - non_competitive_r) / 2,
              label = glue::glue("Competitive states: ",
                                 538 - non_competitive_d - non_competitive_r,
                                 " votes"),
              family = "IBM Plex Sans",
              color = "#363a3c",
              size = 3.25) +
    scale_fill_identity() +
    coord_flip() +
    theme_void() +
    expand_limits(x = c(-1, 1))

  # bind together
  map_full <-
    (map_ratings / map_bar) +
    plot_layout(heights = c(9, 1))
}

#' Interpolate between an evenly distributed color palette
#'
#' @param x probability that Harris wins in a given state
#' @param pal color palette to interpolate through
interpolate_fill <- function(x, pal) {

  color_rgb <- colorRamp(rev(pal))
  rgb_vals <- color_rgb(x)
  hex <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)

  return(hex)

}

#' Render the interactive map generated by `plot_map()`
#'
#' @param ggobj the plot generated by `plot_map()`
render_interactive_map <- function(ggobj) {

  css <-
    paste("background-color:#fafafa",
          "padding:5px",
          "border-radius:3px",
          "border-color:black",
          "border-style:solid",
          "border-width:0.125em 0em 0em 0em",
          "box-shadow: 0px 0px 10px 5px lightgray",
          sep = ";")

  girafe(
    ggobj = ggobj,
    options = list(
      opts_tooltip(
        css = css,
        opacity = 1
      ),
      opts_hover(
        css = "opacity:1;"
      ),
      opts_hover_inv(
        css = "opacity:0.1;transition:opacity 1s;"
      )
    )
  )

}


