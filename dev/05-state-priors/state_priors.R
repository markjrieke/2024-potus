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
gdp <- fetch_gdp()
cpi <- fetch_cpi()

# wrangle ----------------------------------------------------------------------

# get pvi of each state for a given year
state_results <-
  states %>%
  select(-notes) %>%
  mutate(across(democratic:other, ~.x/100)) %>%
  left_join(abramovitz %>% select(year, dem, rep),
            by = "year") %>%
  mutate(oth = 1 - (dem + rep),
         pvi_3d = democratic - dem,
         pvi_3r = republican - rep,
         pvi_3o = other - oth) %>%
  select(-c(dem, rep, oth))

# get the average pvi for the first two elections each state appears in
first_state_pvi  <-
  state_results %>%
  group_by(state) %>%
  arrange(year) %>%
  slice_head(n = 2) %>%
  summarise(first_pvi_3d = mean(pvi_3d),
            first_pvi_3r = mean(pvi_3r),
            first_pvi_3o = mean(pvi_3o))

# add in lagged pvi for each state
state_results <-
  state_results %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(pvi_3d = 0.75*lag(pvi_3d, 1) + 0.25*lag(pvi_3d, 2),
         pvi_3r = 0.75*lag(pvi_3r, 1) + 0.25*lag(pvi_3r, 2),
         pvi_3o = 0.75*lag(pvi_3o, 1) + 0.25*lag(pvi_3o, 2)) %>%
  ungroup() %>%
  left_join(first_state_pvi, by = "state") %>%
  mutate(pvi_3d = if_else(is.na(pvi_3d), first_pvi_3d, pvi_3d),
         pvi_3r = if_else(is.na(pvi_3r), first_pvi_3r, pvi_3r),
         pvi_3o = if_else(is.na(pvi_3o), first_pvi_3o, pvi_3o)) %>%
  select(-starts_with("first"))

# join with national data
state_results <-
  abramovitz %>%
  select(year,
         inc_party,
         inc_dem,
         inc_rep,
         inc_approval = fte_net_inc_approval) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_dem == 0 & inc_party == "dem" ~ "inc dem party",
                                inc_rep == 1 ~ "inc rep running",
                                inc_rep == 0 & inc_party == "rep" ~ "inc rep party"),
         inc_approval = inc_approval/100) %>%
  select(year, inc_approval, inc_status) %>%
  right_join(state_results, by = "year")

# prep for modeling ------------------------------------------------------------

R <-
  state_results %>%
  select(democratic, republican, other) %>%
  as.matrix()

state_results <-
  state_results %>%
  mutate(iid = case_match(inc_status,
                          "inc dem running" ~ 1,
                          "inc dem party" ~ 2,
                          "inc rep running" ~ 3,
                          "inc rep party" ~ 4),
         iid = as.integer(iid),
         third_party_flag = as.integer(third_party_flag))

stan_data <-
  list(
    N = nrow(state_results),
    K = ncol(R),
    R = R,
    N_inc_status = 4,
    iid = state_results$iid,
    third_party = state_results$third_party_flag
  )

