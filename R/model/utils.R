# Transforms -------------------------------------------------------------------

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
