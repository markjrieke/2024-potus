table_polls <- function(state = "National",
                        col_b,
                        col_t) {

  # internal renaming for filtering
  state_int <- state

  # import polls conducted in that state
  polls <-
    read_csv("out/polls/polls_out.csv") %>%
    filter(state == state_int)

  # if no polls, return formatted text
  if (nrow(polls) == 0) {

    out <-
      glue::glue(
        "<br>",
        "<span style='display:inline-block;width:60%;'>",
        "*No polls have been conducted in {state}. ",
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
    mutate(biden = Y/K,
           trump = 1 - biden,
           margin = biden - trump) %>%
    select(pollster,
           end_date,
           group,
           mode,
           sample_size = K,
           biden,
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
    fmt_percent(columns = c(biden, trump),
                decimals = 1) %>%
    tab_style(style = list(cell_text(color = "lightgray",
                                     weight = 800)),
              locations = cells_body(columns = margin,
                                     rows = winner == "Tie")) %>%
    tab_style(style = list(cell_text(color = col_b,
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


