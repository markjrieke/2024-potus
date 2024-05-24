# Federal Reserve Economic Data (FRED) -----------------------------------------

# Import CPIAUSCL --- all urban consumers, all items, us city average
# https://fred.stlouisfed.org/series/CPIAUCSL
fetch_cpi <- function() {

  current_date <- Sys.Date()
  last_month <- floor_date(floor_date(current_date, "month") - 1, "month")

  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=",
        last_month,
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )

  return(out)

}

# Import GDP
# https://fred.stlouisfed.org/series/GDP
fetch_gdp <- function() {

  current_date <- Sys.Date()
  last_quarter <- floor_date(floor_date(current_date, "quarter") - 1, "quarter")

  if (last_quarter > mdy("4/1/24")) {
    last_quarter <- mdy("4/1/24")
  }

  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=",
        last_quarter,
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )

  return(out)

}

# FiveThirtyEight --------------------------------------------------------------

# Import most up-to-date Biden net approval topline
fetch_approval <- function() {

  read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv") %>%
    filter(subgroup == "All polls",
           end_date == max(end_date)) %>%
    mutate(net = (approve_estimate - disapprove_estimate)/100) %>%
    pull(net)

}
