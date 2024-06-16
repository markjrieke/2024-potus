#' TODO: DOCUMENT
label_date_ordinal <- function(x) {

  m <- scales::label_date("%B")(x)
  d <- scales::label_ordinal()(day(x))

  out <- paste(m, d)
  out <- if_else(str_detect(out, "NA"), NA_character_, out)

  return(out)

}
