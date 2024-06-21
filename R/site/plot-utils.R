#' TODO: DOCUMENT
annotate_end_date <- function(ymin,
                              ymax,
                              ...,
                              x = mdy("11/5/24"),
                              xend = mdy("11/5/24"),
                              ymult = 560/538,
                              ymult_label = 580/538,
                              linewidth = 0.4,
                              color = "gray20",
                              label_size = 3.5,
                              label_font = "IBM Plex Sans") {

  # endpoint for the segment/label
  yend <- ymult * (ymax - ymin) + ymin
  yend_label <- ymult_label * (ymax - ymin) + ymin

  annotation <-
    list(
      # endline segment
      annotate(geom = "segment",
               x = x,
               xend = xend,
               y = ymin,
               yend = ymult * (ymax - ymin) + ymin,
               linewidth = linewidth,
               color = "gray20"),

      # endline point
      annotate(geom = "point",
               x = x,
               y = ymult * (ymax - ymin) + ymin,
               shape = 25,
               color = color,
               fill = color),

      # endline text
      annotate(geom = "text",
               x = x,
               label = "Election Day",
               y = yend_label,
               size = label_size,
               family = label_font,
               color = color)
    )

  return(annotation)
}

#' TODO: DOCUMENT
annotate_current_date <- function(current_date,
                                  date_pt,
                                  ymin,
                                  ymax,
                                  ...,
                                  ymult = 538/538,
                                  ymult_label = 560/538,
                                  linewidth = 0.4,
                                  color = "gray20",
                                  label_size = 3.5,
                                  label_font = "IBM Plex Sans",
                                  bg.color = "white",
                                  bg.r = 0.3) {

  # endpoint for the segment/label
  yend = ymult * (ymax - ymin) + ymin
  yend_label = ymult_label * (ymax - ymin) + ymin

  annotation <-
    list(
      # current date segment
      geom_segment(aes(x = current_date,
                       xend = current_date),
                   y = ymin,
                   yend = yend,
                   linewidth = linewidth,
                   color = color),

      # current date point
      geom_point(aes(x = current_date),
                 y = yend,
                 shape = 25,
                 color = color,
                 fill = color),

      # current date text
      geom_shadowtext(aes(label = date_pt),
                      y = yend_label,
                      size = label_size,
                      family = label_font,
                      color = color,
                      bg.color = bg.color,
                      bg.r = bg.r)
    )

  return(annotation)

}

#' TODO: DOCUMENT
geom_underline <- function(mapping,
                           color,
                           ...,
                           under_color = "white",
                           under_width = 2.5,
                           over_width = 0.8) {

  geom <-
    list(
      # underline
      geom_line(mapping = mapping,
                color = under_color,
                linewidth = under_width),

      # overline
      geom_line(mapping = mapping,
                color = color,
                linewidth = over_width)
    )

  return(geom)

}

#' TODO: DOCUMENT
geom_current_text <- function(mapping,
                              y,
                              color,
                              ...,
                              nudge_x = 10,
                              family = "IBM Plex Sans",
                              fontface = "bold",
                              size = 5,
                              bg.color = "white",
                              bg.r = 0.2) {

  geom_shadowtext(mapping = mapping,
                  y = y,
                  nudge_x = nudge_x,
                  family = family,
                  fontface = fontface,
                  size = size,
                  color = color,
                  bg.color = bg.color,
                  bg.r = bg.r)

}

#' TODO: DOCUMENT
geom_tooltip <- function(mapping,
                         height,
                         ...,
                         width = 1,
                         size = 3,
                         alpha = 0.001) {

  geom_tile_interactive(mapping = mapping,
                        width = width,
                        height = height,
                        size = size,
                        alpha = alpha)

}

#' TODO: DOCUMENT
theme_2024 <- function(ymin,
                       ymax,
                       breaks,
                       labels,
                       xlims = NULL,
                       ylims = NULL,
                       ...,
                       base_size = 12,
                       linewidth = 0.25,
                       vjust = -0.5,
                       hjust = 2,
                       halign = 1,
                       ymult = 580/538) {

  # fill in limits if not supplied
  x_expand <- if (is.null(xlims)) mdy(c("4/20/24", "11/15/24")) else xlims
  y_expand <- if (is.null(ylims)) c(ymin, ymult * (ymax - ymin) + ymin) else ylims

  dressing <-
    list(
      scale_fill_identity(),
      ggh4x::coord_axes_inside(labels_inside = TRUE,
                               yintercept = ymin),
      scale_x_date(breaks = "month", labels = scales::label_date("%B")),
      scale_y_continuous(breaks = breaks,
                         labels = labels,
                         position = "right"),
      theme_rieke(base_size = base_size),
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(linewidth = linewidth),
            axis.ticks.x = element_line(),
            axis.text.y = ggtext::element_markdown(vjust = vjust,
                                                   hjust = hjust,
                                                   halign = halign)),
      labs(x = NULL,
           y = NULL),
      expand_limits(x = x_expand,
                    y = y_expand)
    )

  return(dressing)

}

#' TODO: document
render_interactive <- function(ggobj) {

  css <-
    paste(
      "font-family:IBM Plex Sans",
      "color:#363a3c",
      "background-color:#fafafa",
      "padding:5px",
      "border-radius:3px",
      "border-color:black",
      "border-style:solid",
      "border-width:0.125em 0em 0em 0em",
      "box-shadow: 0px 0px 10px 5px lightgray",
      "min-width: 120px",
      sep = ";"
    )

  girafe(
    ggobj = ggobj,
    options = list(
      opts_tooltip(
        css = css,
        opacity = 1
      ),
      opts_sizing(rescale = TRUE, width = 1)
    )
  )

}


