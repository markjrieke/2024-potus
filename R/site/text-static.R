#' TODO: DOCUMENT
blurb_evs <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Estimated electoral college votes</span>",
    "Our model is updated every day and combines state and national polls with economic indicators to predict a range of outcomes.",
    "The midpoint is the estimate of the electoral college vote for each party on election day.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_prob <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Presidential probabilities</span>",
    "Each day, the model simulates thousands of plausible election results.",
    "Each candidate's probability of winning is the proportion of simulations that they've won.",
    "This is an additional sentence so that the temp blurb takes up the desired amount of space.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_map <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Chance of winning each state</span>",
    "Our model combines the national prediction with polls and political-economic factors at the state level.",
    "We take into account that states that are similar are likely to move with each other; if Donald Trump wins Minnesota,",
    "he will probably win Wisconsin too.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_vote <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>Forecasted voteshare on each day</span>",
    "The model first averages the polls, weighting them by their sample sizes and correcting them for tendencies to overestimate support for one party.",
    "It then combines this average with our forecast based on non-polling data, pulling vote shares on each day slightly towards the final election-day projection.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
blurb_similarity <- function(font = "Playfair Display") {

  glue::glue(
    "### <span style='font-family:{font};'>State similarities</span>",
    "Our model also simulates what would happen if the race moves, or if the polls are biased, in similar amounts in like states.",
    "We calculate similarity between states by comparing their demographic and political profiles, such as the share of white voters who live there, how religious they are, and how urban or rural the state is.",
    .sep = "\n"
  )

}

#' TODO: DOCUMENT
footer <- function() {

  glue::glue(
    "<span style='color:gray;font-size:12px'>",
    "Sources: Ballotpedia; Cook Political Report; The Economist; Federal Reserve Bank of St. Louis; FiveThirtyEight; Urban Stats; 270towin.com",
    "<br>",
    "<br>",
    "Forecast by Mark Rieke",
    "<br>",
    "Copyediting by [D Rieke](https://x.com/drekrek)",
    "</span>",
    .sep = "\n"
  )

}






