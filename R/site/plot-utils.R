#' Add static plot annotation for election day
#'
#' @param ymin,ymax plot boundaries
#' @param ... unused
#' @param x,xend segment annotation start/end
#' @param ymult,ymult_label multipliers used to position the segment endpoint
#'                          and label positions, respectively
#' @param linewidth segment linewidth
#' @param color color applied to all annotation elements
#' @param label_size size of the label
#' @param label_font font used for the label
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

#' Add static plot annotation for the current day
#'
#' @param current_date the current date
#' @param date_pt formatted date string used to label the current date
#' @param ymin,ymax plot boundaries
#' @param ... unused
#' @param ymult,ymult_label multipliers used to position the segment endpoint
#'                          and label positions, respectively
#' @param linewidth segment linewidth
#' @param color color applied to all annotation elements
#' @param label_size size of the label
#' @param label_font font used for the label
#' @param bg.color,bg.r label parameters passed to `shadowtext::geom_shadowtext()`
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

#' Add a `geom_line()` with and underline to a plot
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`
#' @param color color to be passed to `ggplot2::geom_line()`
#' @param ... unused
#' @param under_color color of the underline
#' @param under_width,over_width linewidths of the underline and overline,
#'                               respectively
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

#' Add a text label for current-day estimates
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`
#' @param y y-location to place the label
#' @param color color for the text
#' @param ... unused
#' @param nudge_x label offset in the x-direction
#' @param family label font
#' @param fontface label weight/face
#' @param size label size
#' @param bg.color,bg.r label parameters passed to `shadowtext::geom_shadowtext()`
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

#' Create an invisible tiling used to pass data to an interactive tooltip
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`
#' @param height height passed to `ggiraph::geom_tile_interactive()`
#' @param ... unused
#' @param width width passed to `ggiraph::geom_tile_interactive()`
#' @param size size passed to `ggiraph::geom_tile_interactive()`
#' @param alpha alpha passed to `ggiraph::geom_tile_interactive()`
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

#' Custom theme used in plots across the site
#'
#' @param ymin,ymax plot boundaries
#' @param breaks breaks along the y-axis
#' @param labels labels along the y-axis
#' @param xlims,ylims limits passed to `ggplot2::expand_limits()`
#' @param ... unused
#' @param base_size base_size passed to `riekelib::theme_rieke()`
#' @param linewidth linewidth passed to the major y grid
#' @param vjust vertical adjustment for y-axis text
#' @param hjust horizontal adjustment for y-axis text
#' @param halign horizontal alignment for y-axis text
#' @param ymult multiplier used to set plot expansion limits if `ylims` is not
#'              supplied
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
  x_expand <- if (is.null(xlims)) mdy(c("7/25/24", "11/10/24")) else xlims
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

#' Render an interactive plot as a html object
#'
#' @param ggobj ggplot2 plot to render as interactive
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


