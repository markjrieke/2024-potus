# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(patchwork)

pal <-
  c("#3579ac",
    "#7cb0d7",
    "#d3e5f2",
    "#f2f2f2",
    "#f2d5d5",
    "#d78080",
    "#b13737")

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

# prep for training ------------------------------------------------------------

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
                ~rethinking::standardize(.x)),
         inc_status = if_else(inc_running == 1, 1, 2),
         inc_status = as.integer(inc_status)) %>%
  select(-inc_running)

model_data %>%
  write_csv("dev/07-abramovitz-3pv/model_data.csv")

R <-
  model_data %>%
  select(inc_pct,
         non_inc_pct,
         oth_pct) %>%
  as.matrix()

# prep 2024 approval/gdp -------------------------------------------------------

biden_gdp <-
  real_gdp %>%
  mutate(real_gdp_growth = real_gdp/lag(real_gdp, 4) - 1) %>%
  filter(date <= mdy("7/1/24")) %>%
  filter(date == max(date)) %>%
  pull(real_gdp_growth)

biden_gdp <-
  (biden_gdp - mean(abramovitz$real_gdp_growth))/sd(abramovitz$real_gdp_growth)

biden_approval <-
  approval %>%
  filter(end_date == max(end_date)) %>%
  mutate(net = approve_estimate - disapprove_estimate) %>%
  pull(net)

biden_approval <-
  (biden_approval - mean(abramovitz$fte_net_inc_approval))/sd(abramovitz$fte_net_inc_approval)

# model ------------------------------------------------------------------------

n_samples <- 4000

prior_data <-
  list(
    N = nrow(R),
    K = ncol(R),
    R = R,
    inc_status = model_data$inc_status,
    inc_approval = model_data$inc_approval,
    third_party = model_data$third_party,
    real_gdp_growth = model_data$real_gdp_growth,
    biden_approval = biden_approval,
    biden_gdp = biden_gdp
  )

national_model <-
  cmdstanr::cmdstan_model("dev/07-abramovitz-3pv/national_prior.stan")

national_fit <-
  national_model$sample(
    data = prior_data,
    seed = 2024,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = n_samples/4,
    iter_sampling = n_samples/4
  )

# national prior ---------------------------------------------------------------

national_fit$draws(variables = "prob_gq", format = "df") %>%
  as_tibble() %>%
  select(.draw, starts_with("prob")) %>%
  pivot_longer(starts_with("prob"),
               names_to = "party",
               values_to = "pct") %>%
  mutate(party = parse_number(party),
         party = case_match(party,
                            1 ~ "Dem",
                            2 ~ "Rep",
                            3 ~ "Other"),
         party = fct_relevel(party, c("Other", "Rep", "Dem"))) %>%
  ggplot(aes(x = party,
             y = pct,
             fill = party)) +
  ggdist::stat_histinterval(slab_alpha = 0.5,
                            .width = c(0.5, 0.8, 0.95),
                            breaks = seq(from = 0,
                                         to = 0.75,
                                         length.out = 100)) +
  scale_y_percent() +
  scale_fill_manual(values = c("gray20", pal[7], pal[1])) +
  coord_flip() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior for the National Popular Vote**",
       subtitle = paste("Based on incumbency, net approval, real GDP growth,",
                        "and the presence of third parties",
                        sep = "<br>"),
       x = NULL,
       y = NULL,
       caption = glue::glue("Pointrange indicates 50/80/95% posterior",
                            "credible intervals based on {scales::label_comma()(n_samples)} MCMC samples",
                            .sep = "<br>"))

ggquicksave("dev/07-abramovitz-3pv/abramovitz_3pv_01.png")

# state results ----------------------------------------------------------------

# get current state 3pvi
state_pvi <-

  # join state/national data together
  states %>%
  select(year,
         state,
         democratic,
         republican,
         other) %>%
  mutate(across(-c(year, state), ~.x/100)) %>%
  left_join(abramovitz %>%
              select(year,
                     nat_dem = dem,
                     nat_rep = rep)) %>%

  # get election-specific 3pvi
  mutate(nat_oth = 1 - (nat_dem + nat_rep),
         pvi_3d = democratic - nat_dem,
         pvi_3r = republican - nat_rep,
         pvi_3o = other - nat_oth) %>%

  # get the forward looking 3pvi
  group_by(state) %>%
  arrange(year) %>%
  mutate(year = year + 4,
         across(starts_with("pvi"), ~0.75*.x + 0.25*lag(.x, 1))) %>%
  ungroup() %>%
  filter(year == 2024) %>%
  select(state, starts_with("pvi"))

# summarise with state data
state_draws <-
  national_fit$draws(variables = "prob_gq", format = "df") %>%
  as_tibble() %>%
  select(.draw, starts_with("prob")) %>%
  rename(dem = `prob_gq[1]`,
         rep = `prob_gq[2]`,
         oth = `prob_gq[3]`) %>%
  nest(data = everything()) %>%
  bind_cols(state_pvi, .) %>%
  unnest(data) %>%
  mutate(dem = dem + pvi_3d,
         rep = rep + pvi_3r,
         oth = oth + pvi_3o) %>%
  select(-starts_with("pvi")) %>%
  mutate(across(c(dem, rep, oth),
                ~case_when(.x < 0 ~ 0,
                           .x > 1 ~ 1,
                           TRUE ~ .x)),
         across(c(dem, rep, oth),
                ~.x/(dem + rep + oth)))

state_draws %>%
  mutate(delta = dem - rep) %>%
  group_by(state) %>%
  mutate(dem_win = if_else(delta > 0, 1, 0),
         dem_win = sum(dem_win)/n_samples,
         dem_win = scales::label_percent(accuracy = 1)(dem_win),
         state = paste(state, dem_win)) %>%
  tidybayes::median_qi(delta, .width = c(0.5, 0.8, 0.95)) %>%
  mutate(state = fct_reorder(state, delta)) %>%
  ggplot(aes(x = state,
             y = delta,
             ymin = .lower,
             ymax = .upper,
             color = delta)) +
  ggdist::geom_pointinterval() +
  scale_y_percent() +
  scale_color_gradientn(colors = c(pal[7:6], pal[2:1]),
                        limits = c(-1, 1)) +
  coord_flip() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior Statewide Democratic Margin**",
       subtitle = paste("Based on incumbency, net approval, real GDP growth,",
                        "and the presence of third parties",
                        sep = "<br>"),
       x = NULL,
       y = NULL,
       caption = glue::glue("Pointrange indicates 50/80/95% posterior",
                            "credible intervals based on {scales::label_comma()(n_samples)} MCMC samples",
                            .sep = "<br>"))

ggquicksave("dev/07-abramovitz-3pv/abramovitz_3pv_02.png",
            width = 9,
            height = 12)

# electoral college distribution -----------------------------------------------

ec_axis <-
  tibble(breaks = seq(0, 538, length.out = 11)) %>%
  mutate(color = case_when(breaks > 269 ~ pal[7],
                           breaks < 269 ~ pal[1],
                           TRUE ~ "gray60"),
         labels = case_when(breaks >= 269 ~ round(breaks),
                            breaks < 269 ~ 538 - round(breaks)),
         labels = color_text(labels, color))

ec_draws <-
  state_draws %>%
  left_join(electors) %>%
  mutate(winner = if_else(dem > rep, 1, 0),
         dem_electors = winner*electors) %>%
  group_by(.draw) %>%
  summarise(electors = sum(dem_electors)) %>%
  mutate(fill = case_when(electors > 269 ~ pal[1],
                          electors < 269 ~ pal[7],
                          TRUE ~ "gray40"),
         electors = 538 - electors)

biden_prob <-
  ec_draws %>%
  percent(fill) %>%
  filter(fill == pal[1]) %>%
  pull(pct)

ec_draws %>%
  ggplot(aes(x = electors,
             fill = fill)) +
  geom_histogram(binwidth = 1) +
  geom_hline(yintercept = 0,
             color = "gray60") +
  scale_fill_identity() +
  scale_x_continuous(breaks = ec_axis$breaks,
                     labels = ec_axis$labels) +
  theme_rieke() +
  theme(axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "**Prior model of Electoral College results**",
       subtitle = glue::glue("Based on incumbency, net approval, and GDP growth,",
                             "Biden has a **{scales::label_percent(accuracy = 1)(biden_prob)}** chance of winning re-election",
                             .sep = "<br>"),
       x = NULL,
       y = NULL) +
  expand_limits(x = c(0, 538))

ggquicksave("dev/07-abramovitz-3pv/abramovitz_3pv_03.png")

# ec rating map ----------------------------------------------------------------

ec_summary <-
  state_draws %>%
  mutate(winner = if_else(dem > rep, 1, 0)) %>%
  group_by(state) %>%
  summarise(pct = sum(winner)/n_samples) %>%
  left_join(electors) %>%
  mutate(rating = case_when(pct > 0.99 ~ pal[1],
                            pct > 0.85 ~ pal[2],
                            pct > 0.65 ~ pal[3],
                            pct >= 0.35 ~ pal[4],
                            pct >= 0.15 ~ pal[5],
                            pct >= 0.01 ~ pal[6],
                            TRUE ~ pal[7])) %>%
  group_by(rating) %>%
  summarise(electors = sum(electors))

# legend at the top
map_legend <-
  ec_summary %>%
  left_join(tibble(rating = pal,
                   rating_text = c("Safe Dem",
                                   "Very likely Dem",
                                   "Likely Dem",
                                   "Uncertain",
                                   "Likely Rep",
                                   "Very Likely Rep",
                                   "Safe Rep"))) %>%
  mutate(rating = fct_relevel(rating, pal)) %>%
  ggplot(aes(x = rating,
             y = 0)) +
  geom_point(aes(fill = rating,
                 color = if_else(rating_text == "Uncertain", "black", "white")),
             shape = 21,
             size = 4) +
  geom_text(aes(label = rating_text),
            family = "IBM Plex Sans",
            fontface = "bold",
            size = 3,
            vjust = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(plot.title = ggtext::element_markdown(family = "Tiempos Text",
                                              size = 18,
                                              color = "gray20")) +
  labs(title = "**Prior Electoral College Ratings**") +
  expand_limits(x = c(0.5, 7.5))

# state points on map
map_points <-
  state_draws %>%
  mutate(delta = dem - rep) %>%
  group_by(state) %>%
  mutate(dem_win = if_else(delta > 0, 1, 0)) %>%
  summarise(dem_win = sum(dem_win)/n_samples) %>%
  left_join(electors) %>%
  mutate(rating = case_when(dem_win > 0.99 ~ pal[1],
                            dem_win > 0.85 ~ pal[2],
                            dem_win > 0.65 ~ pal[3],
                            dem_win >= 0.35 ~ pal[4],
                            dem_win >= 0.15 ~ pal[5],
                            dem_win >= 0.01 ~ pal[6],
                            TRUE ~ pal[7])) %>%
  left_join(centers) %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326)

us_geo <-
  tigris::states(class = "sf", cb = TRUE) %>%
  tigris::shift_geometry() %>%
  filter(GEOID < 60)

map_ratings <-
  us_geo %>%
  ggplot() +
  geom_sf(fill = "#fafafa",
          color = "#ededed",
          linewidth = 0.5) +
  geom_sf(data = map_points,
          aes(size = electors,
              fill = rating,
              color = if_else(rating == pal[4], "black", "white")),
          shape = 21) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_size_continuous(range = c(3, 12)) +
  theme_void() +
  theme(legend.position = "none")

map_bar <-
  ec_summary %>%
  mutate(rating = fct_relevel(rating, pal[7:1])) %>%
  arrange(desc(rating)) %>%
  mutate(label_pos = cumsum(electors) - electors/2) %>%
  ggplot(aes(x = 0,
             y = electors,
             fill = rating)) +
  geom_bar(position = "stack",
           stat = "identity") +
  geom_segment(x = -0.5,
               xend = 0.5,
               y = 269,
               yend = 269,
               color = "gray60") +
  geom_text(aes(y = label_pos,
                label = electors),
            family = "IBM Plex Sans",
            fontface = "bold",
            size = 4) +
  scale_fill_identity() +
  coord_flip() +
  theme_void() +
  expand_limits(x = c(-1, 1))

(map_legend / map_ratings / map_bar) +
  plot_layout(heights = c(1, 9, 1))

ggquicksave("dev/07-abramovitz-3pv/abramovitz_3pv_04.png",
            width = 7.2,
            height = 6.2)



