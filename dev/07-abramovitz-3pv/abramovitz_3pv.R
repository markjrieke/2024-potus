# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# fred functions ---------------------------------------------------------------

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

fetch_gdp <- function() {

  current_date <- Sys.Date()

  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )

  return(out)

}

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
states <- read_csv("data/statewide_results.csv")
approval <- read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv")
electors <- read_csv("data/electors.csv")
centers <- read_csv("data/state_centers.csv")
cpi <- fetch_cpi()
gdp <- fetch_gdp()

# wrangle economic data --------------------------------------------------------

max_cpi <-
  cpi %>%
  filter(DATE == max(DATE)) %>%
  pull(CPIAUCSL)

cpi <-
  cpi %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(inflation = max_cpi/cpi)

# adjust gdp for inflation
real_gdp <-
  gdp %>%
  rename_with(str_to_lower) %>%
  left_join(cpi) %>%
  select(-cpi) %>%
  mutate(real_gdp = gdp*inflation) %>%
  select(date, real_gdp)

# append national data with 2nd quarter annualized real gdp growth
abramovitz <-
  abramovitz %>%
  mutate(begin_quarter = mdy(paste0("7/1/", year - 1)),
         end_quarter = mdy(paste0("7/1/", year))) %>%
  left_join(real_gdp, by = c("begin_quarter" = "date")) %>%
  rename(begin_gdp = real_gdp) %>%
  left_join(real_gdp, by = c("end_quarter" = "date")) %>%
  rename(end_gdp = real_gdp) %>%
  mutate(real_gdp_growth = end_gdp/begin_gdp - 1) %>%
  select(-ends_with("quarter"),
         -ends_with("gdp"))

# prep for modeling ------------------------------------------------------------

model_data <-
  abramovitz %>%
  mutate(inc_pct = if_else(inc_party == "dem", dem, rep),
         non_inc_pct = if_else(inc_party == "dem", rep, dem),
         oth_pct = 1 - (inc_pct + non_inc_pct)) %>%
  select(inc_pct,
         non_inc_pct,
         oth_pct,
         inc_running,
         inc_approval = fte_net_inc_approval,
         real_gdp_growth,
         third_party = third_party_present) %>%
  mutate(across(c(inc_approval, real_gdp_growth),
                ~rethinking::standardize(.x)))

model_data %>%
  write_csv("dev/07-abramovitz-3pv/model_data.csv")

R <-
  model_data %>%
  select(inc_pct,
         non_inc_pct,
         oth_pct) %>%
  as.matrix()



















