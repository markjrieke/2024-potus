#' Generate formatted static text
#'
#' @description
#' Generate formatted static text that is reused throughout the site to describe
#' charts. `blurb_evs()` and `blurb_map()` are unique to the national page.
#' `blurb_similarity()` only appears on state pages. All others appear on both
#' the national and state pages
#'
#' @param font header font for the blurb title
#'
#' @name static-blurbs
NULL

#' @rdname static-blurbs
blurb_evs <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Projected electoral college votes</span>",
    "The model is updated daily, blending state and national polls with non-polling predictors, like economic growth and presidential approval, to generate a range of potential outcomes in the electoral college.",
    "As we get closer to election day, the uncertainty around the estimate will decrease.",
    .sep = "\n"
  )

}

#' @rdname static-blurbs
blurb_prob <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Presidential probabilities</span>",
    "Each day, the model simulates thousands of plausible election results, from landslide victories to tightly contested races.",
    "Each candidate’s probability of winning is the proportion of simulations that they’ve won.",
    .sep = "\n"
  )

}

#' @rdname static-blurbs
blurb_map <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Chance of winning each state</span>",
    "State-level results determine the makeup of the electoral college.",
    "Most states heavily favor a particular party, leaving a few competitive battlegrounds that will be decisive in determining the next president.",
    "Hover/click to see more information about a particular state.",
    .sep = "\n"
  )

}

#' @rdname static-blurbs
blurb_vote <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Forecasted election-day voteshare</span>",
    "The model first constructs a polling average, pooling data across similar states when polls are sparse.",
    "It then projects forward to election day, initially relying on non-polling indicators like economic growth and partisanship, but aligning more closely with the polling average as election day approaches.",
    .sep = "\n"
  )

}

#' @rdname static-blurbs
blurb_conditional <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Conditional outcomes</span>",
    "From the thousands of simulations, the model can see how the electoral college outcome changes when each candidate wins in a specific state.",
    "If Harris wins in a red-leaning state, for example, it's likelier that she also wins in competitive states.",
    .sep = "\n"
  )

}

#' @rdname static-blurbs
blurb_similarity <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>State similarities</span>",
    "The model uses state characteristics, like demographic composition, population density, and education, to estimate how similar states are to one another.",
    "Similar states are more likely to share polling biases and see similar shifts in polling trendlines.",
    .sep = "\n"
  )

}

#' Generate formatted footer text
#'
#' @description
#' Generates formatted footer text that cites sources and links to the source
#' code/data in the repository.
#'
#' @param branch github branch to extract data from. Defaults to `"dev"`.
footer <- function(branch = "dev") {

  gh_icon <- "{{< fa brands github >}}"
  data_icon <- "{{< fa solid database >}}"

  glue::glue(
    "<span style='color:gray;font-size:12px'>",
    "Sources: Ballotpedia; Cook Political Report; The Economist; Federal Reserve Bank of St. Louis; FiveThirtyEight; Urban Stats; 270towin.com",
    "<br>",
    "<br>",
    "[{gh_icon} View the source code](https://github.com/markjrieke/2024-potus/tree/{branch})",
    "<br>",
    "[{data_icon} Explore the output](https://github.com/markjrieke/2024-potus/tree/{branch}/out)",
    "</span>",
    .sep = "\n"
  )

}





