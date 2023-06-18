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

# append national data with real gdp growth
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

# quick plot!
abramovitz %>%
  ggplot(aes(x = year,
             y = real_gdp_growth,
             fill = inc_party)) +
  geom_col(alpha = 0.75) +
  scale_fill_manual(values = c("royalblue", "red")) +
  scale_y_percent() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**2nd Quarter Annualized Real GDP Growth**",
       subtitle = glue::glue("In election years with an incumbent ",
                             "**{color_text('Democrat', 'royalblue')}** or ",
                             "**{color_text('Republican', 'red')}**"),
       x = NULL,
       y = NULL)

ggquicksave("dev/05-state-priors/real_gdp_growth.png")

# wrangle state data -----------------------------------------------------------

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
         inc_approval = fte_net_inc_approval,
         real_gdp_growth) %>%
  mutate(inc_status = case_when(inc_dem == 1 ~ "inc dem running",
                                inc_dem == 0 & inc_party == "dem" ~ "inc dem party",
                                inc_rep == 1 ~ "inc rep running",
                                inc_rep == 0 & inc_party == "rep" ~ "inc rep party"),
         inc_approval = inc_approval/100) %>%
  select(year, inc_approval, inc_status, real_gdp_growth) %>%
  right_join(state_results, by = "year")

####################### WORK TO DO BRUH START HERE #############################

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

# prior model ------------------------------------------------------------------

sims <- 1000
sigma <- 0.25

even <- 0.475
inc_bonus <- 0.025
wt <- 2

# sim raw parameters
bind_cols(# incumbency status
          idr_1 = rnorm(sims, (even + inc_bonus)*wt, sigma),
          idr_2 = rnorm(sims, (even - inc_bonus)*wt, sigma),
          idp_1 = rnorm(sims, wt*0.5, sigma),
          idp_2 = rnorm(sims, wt*0.5, sigma),
          irr_1 = rnorm(sims, (even - inc_bonus)*wt, sigma),
          irr_2 = rnorm(sims, (even + inc_bonus)*wt, sigma),
          irp_1 = rnorm(sims, wt*0.5, sigma),
          irp_2 = rnorm(sims, wt*0.5, sigma),

          # third party
          third_party_1 = rnorm(sims, -0.35, sigma),
          third_party_2 = rnorm(sims, -0.35, sigma),

          # approval
          # assumption is that as approval increases, both d/r will increase
          # (i.e., less approval = more 3rd party voters)
          # more of the disapproval handled by interaction later
          net_app_1 = rnorm(sims, 0.125, sigma),
          net_app_2 = rnorm(sims, 0.125, sigma),

          # interaction between incumbency status//net approval
          idr_app_1 = rnorm(sims, 0.25, sigma),
          idr_app_2 = rnorm(sims, -0.25, sigma),
          idp_app_1 = rnorm(sims, 0.1, sigma),
          idp_app_2 = rnorm(sims, -0.1, sigma),
          irr_app_1 = rnorm(sims, -0.25, sigma),
          irr_app_2 = rnorm(sims, 0.25, sigma),
          irp_app_1 = rnorm(sims, -0.1, sigma),
          irp_app_2 = rnorm(sims, 0.1, sigma),

          # local pvi,
          pvi_3d_1 = rnorm(sims, 1, sigma),
          pvi_3d_2 = rnorm(sims, -1, sigma),
          pvi_3r_1 = rnorm(sims, -1, sigma),
          pvi_3r_2 = rnorm(sims, 1, sigma),
          pvi_3o_1 = rnorm(sims, -1, sigma),
          pvi_3o_2 = rnorm(sims, -1, sigma)) %>%

  # sum-to-zero constraint for 3rd party/other
  mutate(idr_3 = -(idr_1 + idr_2),
         idp_3 = -(idp_1 + idp_2),
         irr_3 = -(irr_1 + irr_2),
         irp_3 = -(irp_1 + irp_2),
         net_app_3 = -(net_app_1 + net_app_2),
         third_party_3 = -(third_party_1 + third_party_2),
         idr_app_3 = -(idr_app_1 + idr_app_2),
         idp_app_3 = -(idp_app_1 + idp_app_2),
         irr_app_3 = -(irr_app_1 + irr_app_2),
         irp_app_3 = -(irp_app_1 + irp_app_2),
         pvi_3d_3 = -(pvi_3d_1 + pvi_3d_2),
         pvi_3r_3 = -(pvi_3r_1 + pvi_3r_2),
         pvi_3o_3 = -(pvi_3o_1 + pvi_3o_2)) %>%
  nest(data = everything()) %>%

  bind_cols(state_results,
            prior_sims = .) %>%
  rowid_to_column() %>%
  unnest(data) %>%

  # apply linear model
  mutate(# independent of incumbency
         out_1 = pvi_3d_1*pvi_3d + pvi_3r_1*pvi_3r + pvi_3o_1*pvi_3o + third_party_1*third_party_flag,
         out_2 = pvi_3d_2*pvi_3d + pvi_3r_2*pvi_3r + pvi_3o_2*pvi_3o + third_party_2*third_party_flag,
         out_3 = pvi_3d_3*pvi_3d + pvi_3r_3*pvi_3r + pvi_3o_3*pvi_3o + third_party_3*third_party_flag,

         # incumbency param
         inc_1 = case_when(inc_status == "inc dem running" ~ idr_1,
                           inc_status == "inc dem party" ~ idp_1,
                           inc_status == "inc rep running" ~ irr_1,
                           inc_status == "inc rep party" ~ irp_1),
         inc_2 = case_when(inc_status == "inc dem running" ~ idr_2,
                           inc_status == "inc dem party" ~ idp_2,
                           inc_status == "inc rep running" ~ irr_2,
                           inc_status == "inc rep party" ~ irp_2),
         inc_3 = case_when(inc_status == "inc dem running" ~ idr_3,
                           inc_status == "inc dem party" ~ idp_3,
                           inc_status == "inc rep running" ~ irr_3,
                           inc_status == "inc rep party" ~ irp_3),

         out_1 = out_1 + inc_1,
         out_2 = out_2 + inc_2,
         out_3 = out_3 + inc_3) %>%

  # convert to probabilities
  mutate(prob = pmap(list(out_1, out_2, out_3), ~softmax(c(..1, ..2, ..3)))) %>%

  # extract probabilities
  mutate(prob_1 = map_dbl(prob, ~.x[1]),
         prob_2 = map_dbl(prob, ~.x[2]),
         prob_3 = map_dbl(prob, ~.x[3])) %>%

  # clean up (again)
  select(rowid, starts_with("prob_")) %>%

  # summarise with range
  pivot_longer(starts_with("prob"),
               names_to = "party",
               values_to = "pct") %>%
  group_by(rowid, party) %>%
  summarise(.pred = quantile(pct, probs = 0.5),
            .pred_lower = quantile(pct, probs = 0.05),
            .pred_upper = quantile(pct, probs = 0.95)) %>%
  ungroup() %>%

  # prep for plotting
  mutate(party = case_match(party,
                            "prob_1" ~ "democratic",
                            "prob_2" ~ "republican",
                            "prob_3" ~ "other")) %>%
  left_join(state_results %>%
              select(democratic,
                     republican,
                     other,
                     inc_status,
                     third_party_flag) %>%
              rowid_to_column() %>%
              pivot_longer(c(democratic, republican, other),
                           names_to = "party",
                           values_to = "result")) %>%
  mutate(inc_status = str_to_title(inc_status),
         third_party_flag = paste("Third Party:", third_party_flag)) %>%

  # plot!
  ggplot(aes(x = result,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper,
             color = party)) +
  geom_pointrange(alpha = 0.25) +
  geom_abline(linetype = "dashed",
              linewidth = 0.25) +
  NatParksPalettes::scale_color_natparks_d("Triglav") +
  scale_xy_percent() +
  facet_grid(rows = vars(third_party_flag),
             cols = vars(inc_status)) +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Prior Simulation of Statewide Presidential Results**",
       subtitle = paste("Model based on incumbent status, approval, 3PVI, and the presence of a third party on the ticket",
                        "Points show actual election results",
                        sep = "<br>"),
       x = "Result",
       y = "Prior Prediction",
       caption = "1,000 prior simulations")

ggquicksave("dev/05-state-priors/prior_model.png",
            width = 12,
            height = 8)
