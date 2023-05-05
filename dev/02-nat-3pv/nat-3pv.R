# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
gdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-05-05&revision_date=2023-05-05&nd=1947-01-01")
cpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2023-03-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-05-05&revision_date=2023-05-05&nd=1947-01-01")

# prep -------------------------------------------------------------------------

max_cpi <- max(cpi$CPIAUCSL)

model_gdp <-
  gdp %>%
  left_join(cpi) %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(gdp_adjust = gdp*max_cpi/cpi,
         gdp_growth = (gdp_adjust - lag(gdp_adjust, n = 4))/lag(gdp_adjust, n = 4),
         quarter = quarter(date),
         year = year(date)) %>%
  filter(quarter == 3) %>%
  select(year, gdp_growth)

model_data <-
  abramovitz %>%
  left_join(model_gdp) %>%
  select(dem,
         rep,
         inc_dem,
         inc_rep,
         inc_party,
         inc_running,
         inc_approval = fte_net_inc_approval,
         third_party = third_party_present,
         gdp_growth) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_rep == 1 ~ "inc rep running",
                                inc_running == 0 & inc_party == "dem" ~ "inc dem party",
                                TRUE ~ "inc rep party"),
         other = 1 - dem - rep,
         inc_approval = inc_approval/100) %>%
  relocate(other, .after = rep) %>%
  select(-inc_dem, -inc_rep, -inc_running)

pid <-
  model_data %>%
  distinct(inc_party) %>%
  rowid_to_column("pid")

iid <-
  model_data %>%
  distinct(inc_status) %>%
  arrange(inc_status) %>%
  rowid_to_column("iid")

model_data <-
  model_data %>%
  left_join(pid) %>%
  left_join(iid)

# priors -----------------------------------------------------------------------

gtools::rdirichlet(5000, c(80, 80, 10)) %>%
  as_tibble() %>%
  rename(dem = V1,
         rep = V2,
         other = V3) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name,
             y = value)) +
  ggdist::stat_histinterval() +
  coord_flip()

# model ------------------------------------------------------------------------

R <-
  model_data %>%
  select(dem, rep, other) %>%
  as.matrix()

stan_data <-
  list(
    N = nrow(R),
    R = R
  )

model <- cmdstanr::cmdstan_model("dev/02-nat-3pv/nat_3pv_01.stan")

fit <-
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2024
  )

draws <- fit$draws(variables = "theta", format = "df")

draws %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta"),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = case_match(parameter,
                                "theta[1]" ~ "Democrat",
                                "theta[2]" ~ "Republican",
                                "theta[3]" ~ "Other"),
         fill = case_match(parameter,
                           "Democrat" ~ RColorBrewer::brewer.pal(3, "Set1")[2],
                           "Republican" ~ RColorBrewer::brewer.pal(3, "Set1")[1],
                           "Other" ~ "gray60"),
         parameter = fct_reorder(parameter, value)) %>%
  ggplot(aes(x = parameter,
             y = value,
             fill = fill)) +
  ggdist::stat_histinterval(breaks = seq(from = 0, to = 0.7, by = 0.001)) +
  scale_fill_identity() +
  scale_y_percent() +
  coord_flip() +
  theme_rieke() +
  labs(title = "**Simple mean-only model**",
       x = NULL,
       y = NULL)

ggquicksave("dev/02-nat-3pv/nat_3pv_01.png")
