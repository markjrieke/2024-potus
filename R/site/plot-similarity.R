#' TODO: DOCUMENT
plot_similarity <- function(state,
                            ...,
                            col_low = "white",
                            col_high = "#11c458") {

  # rename aggregate states
  state_int <- str_remove(state, " CD-[:digit:]")

  # import similarity results per state
  similarity <- read_rds("data/features/F_a.rds")
  similarity <-
    similarity[state_int,] %>%
    as_tibble() %>%
    rename(similarity = value) %>%
    bind_cols(state = rownames(similarity)) %>%
    arrange(state)

  # import map data
  us_geo <-
    read_rds("data/tigris/tigris.rds")

  # baseline map
  base <-
    us_geo %>%
    left_join(similarity, by = c("NAME" = "state")) %>%
    mutate(fill = map_chr(similarity, ~interpolate_fill(.x, col_low, col_high)))

  # highlight selected state
  highlight <-
    base %>%
    filter(NAME == state_int)

  # return plot
  base %>%
    ggplot(aes(fill = fill)) +
    geom_sf(color = "#ededed",
            linewidth = 0.5) +
    geom_sf(data = highlight,
            fill = "white",
            color = "black",
            linewidth = 0.7) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_void()

}
