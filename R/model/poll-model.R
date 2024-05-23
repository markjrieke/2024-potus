#' Transform a beta-distributed variable to a normal distribution via a copula
#'
#' @param data tibble of state-level features
#' @param feature feature column to conduct the transformation over
#'
#' @return the same tibble supplied to `data` where the `feature` column is now
#' normally distributed
copula_transform <- function(data, feature) {

  # beta distributed observations
  observed <- data %>% pull({{ feature }})

  # estimate and extract parameters
  fit <-
    gamlss::gamlss(
      observed ~ 1,
      family = gamlss.dist::BEo()
    )

  alpha <- fitted(fit, "mu")[1]
  beta <- fitted(fit, "sigma")[1]

  # convert to normal space
  uniform_space <- pbeta(observed, alpha, beta)
  normal_space <- qnorm(uniform_space)

  # replace in the original data
  out <-
    data %>%
    mutate("{{feature}}" := normal_space)

  return(out)

}

# Mapping ----------------------------------------------------------------------

#' Generate a reference table mapping a feature column to an id
#'
#' @description
#' Mapping id's are generated using the first letter of the feature column. For
#' example, supplying the `pollster` column to `map_ids()` will generate a table
#' mapping each pollster name to a `pid`.
#'
#' @param .data tibble containing features to be mapped
#' @param col feature to be mapped
#'
#' @return a tibble with one row per unique category in `col`
map_ids <- function(.data, col) {

  # get id column name
  colname <- deparse(substitute(col))
  idname <- paste0(str_sub(colname, 1, 1), "id")

  # generate mapping table
  out <-
    .data %>%
    distinct({{ col }}) %>%
    arrange({{ col }}) %>%
    rowid_to_column(idname)

  return(out)

}

