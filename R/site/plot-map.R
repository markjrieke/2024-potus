plot_map <- function() {

  # generate clickable links
  clickables <-
    generate_links()$all[[1]] %>%
    str_split("<br>") %>%
    tibble(md = .) %>%
    unnest(md) %>%
    separate(md, c("state", "link"), "]") %>%
    mutate(state = str_remove(state, "\\["),
           link = str_sub(link, 2, -2),
           link = glue::glue("window.open(\"{link}\")"))

  rating_text <-
    tibble(rating = pal,
           rating_text = c("Safe Dem",
                           "Very likely Dem",
                           "Likely Dem",
                           "Uncertain",
                           "Likely Rep",
                           "Very likely Rep",
                           "Safe Rep"))

  us_geo <-
    read_rds("data/tigris/tigris.rds")

  state_ratings <-
    read_csv("out/polls/win_state.csv") %>%
    filter(run_date == max(run_date)) %>%
    left_join(read_csv("data/static/electors.csv")) %>%
    drop_na() %>%
    mutate(rating = case_when(p_win > 0.99 ~ pal[1],
                              p_win > 0.85 ~ pal[2],
                              p_win > 0.65 ~ pal[3],
                              p_win >= 0.35 ~ pal[4],
                              p_win >= 0.15 ~ pal[5],
                              p_win >= 0.01 ~ pal[6],
                              TRUE ~ pal[7]))

  ec_summary <-
    state_ratings %>%
    group_by(rating) %>%
    summarise(electors = sum(electors))

  map_legend <-
    ec_summary %>%
    left_join(rating_text) %>%
    mutate(rating = fct_relevel(rating, pal)) %>%
    ggplot(aes(x = rating,
               y = 0)) +
    geom_point(aes(fill = rating,
                   color = if_else(rating_text == "Uncertain", "black", "white")),
               shape = 21,
               size = 5) +
    geom_text(aes(label = rating_text),
              family = "IBM Plex Sans",
              size = 3.5,
              vjust = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_void() +
    expand_limits(x = c(0.5, 7.5))

  map_points <-
    state_ratings %>%
    left_join(read_csv("data/static/state_centers.csv")) %>%
    left_join(clickables) %>%
    sf::st_as_sf(coords = c("long", "lat"),
                 crs = 4326) %>%
    mutate(candidate = if_else(p_win >= 0.5, "Biden", "Trump")) %>%
    left_join(rating_text) %>%
    mutate(rating_text = str_to_lower(rating_text),
           rating_text = str_remove_all(rating_text, " dem| rep"),
           rating_text = if_else(rating_text == "safe", "all but guaranteed", rating_text),
           p_label = if_else(candidate == "Biden", p_win, 1 - p_win),
           p_label = if_else(p_label > 0.99, ">99%", scales::label_percent(accuracy = 1)(p_label)),
           ev_label = if_else(electors == 1, "EV", "EVs"),
           text_color = case_when(rating_text == "uncertain" ~ "gray",
                                  candidate == "Biden" ~ pal[1],
                                  TRUE ~ pal[7]),
           summary_text = if_else(rating_text == "uncertain",
                                  glue::glue("<b>{candidate}</b> has a {p_label} chance of winning"),
                                  glue::glue("<b>{candidate}</b> is {rating_text} to win ({p_label})")),
           tooltip = glue::glue("<span style='text-align:left;'><b>{state}</b></span><span style='float:right;font-size:12px;'>({electors} {ev_label})</span>",
                                "<span style='font-size:14px'>{summary_text}</span>",
                                .sep = "<br>"),
           tooltip = glue::glue("{color_text(tooltip, text_color)}"))

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
                            color = if_else(rating == pal[4], "black", "white"),
                            tooltip = tooltip,
                            data_id = state),
                        shape = 21) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size_continuous(range = c(4.5, 18)) +
    theme_void() +
    theme(legend.position = "none")

  map_bar <-
    ec_summary %>%
    mutate(rating = fct_relevel(rating, pal[7:1])) %>%
    arrange(desc(rating)) %>%
    mutate(label_pos = cumsum(electors) - electors/2) %>%
    ggplot(aes(x = 0,
               y = electors,
               fill = rating)) +
    geom_bar(position = "stack",
             stat = "identity") +
    geom_segment(x = -0.5,
                 xend = 0.5,
                 y = 269,
                 yend = 269,
                 color = "gray60") +
    geom_text(aes(y = label_pos,
                  label = electors),
              family = "IBM Plex Sans",
              fontface = "bold",
              size = 4) +
    scale_fill_identity() +
    coord_flip() +
    theme_void() +
    expand_limits(x = c(-1, 1))

  map_full <-
    (map_legend / map_ratings / map_bar) +
    plot_layout(heights = c(1, 9, 1))

}

#' TODO: DOCUMENT
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
    ggobj = ec_map,
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

