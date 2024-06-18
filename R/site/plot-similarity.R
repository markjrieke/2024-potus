#' TODO: DOCUMENT
plot_similarity <- function(state,
                            ...,
                            branch = "dev",
                            col_low = "white",
                            col_high = "#11c458") {

  # rename aggregate states
  state_int <- str_remove(state, " CD-[:digit:]")

  # import similarity results per state
  similarity <- read_rds(find_document("data/features/F_a.rds", branch = branch))
  similarity <-
    similarity[state_int,] %>%
    as_tibble() %>%
    rename(similarity = value) %>%
    bind_cols(state = rownames(similarity)) %>%
    arrange(state)

  # import map data
  us_geo <-
    read_rds(find_document("data/tigris/tigris.rds", branch = branch))

  # baseline map
  base <-
    us_geo %>%
    left_join(similarity, by = c("NAME" = "state")) %>%
    mutate(fill = map_chr(similarity, ~interpolate_fill(.x, col_low, col_high)))

  # highlight selected state
  highlight <-
    base %>%
    filter(NAME == state_int)

  # similarity map
  base %>%
    ggplot(aes(fill = similarity)) +
    geom_sf(color = "#ededed",
            linewidth = 0.5) +
    geom_sf(data = highlight,
            fill = "white",
            color = "black",
            linewidth = 0.7) +
    scale_color_identity() +
    scale_fill_gradientn(colors = c(col_low, col_high),
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("**0**", rep(NA_character_, 3), "**100**"),
                         limits = c(0, 1)) +
    theme_void() +
    theme(legend.position = "top",
          legend.text.position = "bottom",
          legend.title.position = "top",
          legend.title = ggtext::element_markdown(family = "IBM Plex Sans",
                                                  hjust = 0.5),
          legend.text = ggtext::element_markdown(family = "IBM Plex Sans")) +
    guides(fill = guide_legend(title = glue::glue("**Similarity with {state}**"),
                               family = "IBM Plex Sans"))

}
