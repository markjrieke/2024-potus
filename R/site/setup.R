#' Setup and configuration for site-related functions
#'
#' @description
#' Loads necessary libraries & supplies static theming values for re-use
#' throughout the site.
#'

# libraries --------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(patchwork)
library(ggiraph)
library(gt)
library(gtExtras)

# palettes ---------------------------------------------------------------------

# map palette - must be of length == 7
pal <-
  c(
    "#3579ac",
    "#7cb0d7",
    "#d3e5f2",
    "#f2f2f2",
    "#f2d5d5",
    "#d78080",
    "#b13737"
  )

# candidate colors
col_b <- pal[1]
col_t <- pal[7]

# header font across the site
header_font <- "Playfair Display"
