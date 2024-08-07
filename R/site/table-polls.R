#' Generate a paginated table of polls conducted in a given state
#'
#' @description
#' Generates an interactive, paginated table of polls conducted in a given
#' state. By default, polls are arranged by descending `end_date`, but can be
#' sorted by any column in the table. If no polls have been conducted, returns
#' a text blurb explaining how the forecasted voteshare is estimated.
#'
#' @param state state to display polls for
#' @param col_h color for showing Harris' margins in the table
#' @param col_t color for showing Trump's margins in the table
#' @param ... unused
#' @param branch github branch to extract data from. Defaults to `"dev"`.
table_polls <- function(state = "National",
                        col_h,
                        col_t,
                        ...,
                        branch = "dev") {

  # internal renaming for filtering
  state_int <- state

  # import polls conducted in that state
  polls <-
    read_csv(find_document("out/polls/polls_out.csv", branch = branch)) %>%
    filter(state == state_int,
           dem_candidate == "Harris")

  # if no polls, return formatted text
  if (nrow(polls) == 0) {

    # modify state name for D.C.
    state_name <-
      if_else(
        state == "District of Columbia",
        "the District of Columbia",
        state
      )

    out <-
      glue::glue(
        "<br>",
        "<span style='display:inline-block;width:60%;'>",
        "*No polls have been conducted in {state_name}. ",
        "The projected voteshare is estimated using economic and approval indicators, ",
        "as well as polling information from similar states.*",
        "</span>",
        "<br>"
      )

    return(out)

  }

  # else, generate a polling table
  polls_table <-
    polls %>%

    # formatting chnages needed for table
    mutate(harris = Y/K,
           trump = 1 - harris,
           margin = harris - trump) %>%
    select(pollster,
           end_date,
           group,
           mode,
           sample_size = K,
           harris,
           trump,
           margin) %>%
    arrange(desc(end_date)) %>%
    mutate(winner = case_when(near(margin, 0) ~ "Tie",
                              margin > 0 ~ "D",
                              TRUE ~ "R"),
           margin = if_else(winner == "Tie",
                            "-",
                            glue::glue("{winner} +{scales::label_percent(accuracy = 0.1)(abs(margin))}")),
           group = case_match(group,
                              "lv" ~ "Likely voters",
                              "rv" ~ "Registered voters",
                              "v" ~ "Voters",
                              "a" ~ "Adults")) %>%

    # generate table
    gt() %>%

    # rename columns for display
    cols_label(end_date = "date",
               group = "population",
               sample_size = "sample size") %>%

    # recreate gtExtras::gt_theme_nytimes()
    # (doesn't work natively with interactive tables)
    tab_options(heading.align = "left",
                column_labels.border.top.style = "none",
                table.border.top.style = "none",
                column_labels.border.bottom.style = "none",
                column_labels.border.bottom.width = 1,
                column_labels.border.bottom.color = "#334422",
                table_body.border.top.style = "none",
                table_body.border.bottom.color = "white",
                heading.border.bottom.style = "none",
                data_row.padding = px(7),
                column_labels.font.size = px(9)) %>%

    # apply html formatting directly to avoid interactive table limitations
    cols_label_with(
      fn = function(x) {
        gt::html(paste0("<span style ='text-transform:uppercase;",
                        "color:darkgray;",
                        "font-family:Source Sans 3;",
                        "font-size:11px'>",
                        x,
                        "</span>"))
      }
    ) %>%

    # rewrirte label_date_ordinal() in a way that gt will accept
    text_transform(
      fn = function(x) {
        paste(month(x, label = TRUE, abbr = FALSE), scales::label_ordinal()(day(x)))
      },
      locations = cells_body(columns = end_date)
    ) %>%

    # rewrite scales::label_comma() in a way that gt will accept
    text_transform(
      fn = function(x) {
        xchar <- as.character(x)
        xchar <-
          if_else(nchar(xchar) > 3,
                  paste(str_sub(xchar, 1, -4), str_sub(xchar, -3, -1), sep = ","),
                  xchar)
        return(xchar)
      },
      locations = cells_body(columns = sample_size)
    ) %>%

    # apply html formatting directly since interactivity breaks text formatting in gt
    text_transform(
      fn = function(x) {
        glue::glue("<span style='font-family:IBM Plex Sans;font-size:14px;'>{x}</span>")
      },
      locations = cells_body()
    ) %>%

    # apply formatting to pct columns based on the leader
    fmt_percent(columns = c(harris, trump),
                decimals = 1) %>%
    tab_style(style = list(cell_text(color = "lightgray",
                                     weight = 800)),
              locations = cells_body(columns = margin,
                                     rows = winner == "Tie")) %>%
    tab_style(style = list(cell_text(color = col_h,
                                     weight = 800)),
              locations = cells_body(columns = margin,
                                     rows = winner == "D")) %>%
    tab_style(style = list(cell_text(color = col_t,
                                     weight = 800)),
              locations = cells_body(columns = margin,
                                     rows = winner == "R")) %>%
    cols_hide(winner) %>%

    # html options
    tab_options(table.width = pct(100)) %>%
    opt_interactive()

  return(polls_table)

}


