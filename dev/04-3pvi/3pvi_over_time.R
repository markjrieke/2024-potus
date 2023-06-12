# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

abramovitz <- read_csv("data/abramovitz.csv")
statewide <- read_csv("data/statewide_results.csv")

abramovitz %>%
  select(year, dem, rep) %>%
  mutate(oth = 1 - (dem + rep)) %>%
  right_join(statewide) %>%
  mutate(across(c(democratic, republican, other), ~.x/100),
         dem_lean = democratic - dem,
         rep_lean = republican - rep,
         oth_lean = other - oth) %>%
  select(year, state, ends_with("lean")) %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(pvi_3d = 0.75*lag(dem_lean, 1) + 0.25*lag(dem_lean, 2),
         pvi_3r = 0.75*lag(rep_lean, 1) + 0.25*lag(rep_lean, 2),
         pvi_3o = 0.75*lag(oth_lean, 1) + 0.25*lag(oth_lean, 2)) %>%
  ungroup() %>%
  select(-ends_with("lean")) %>%
  drop_na() %>%
  left_join(statewide) %>%
  mutate(across(c(democratic, republican, other), ~.x/100)) %>%
  select(-notes) %>%
  pivot_longer(c(democratic, republican, other),
               names_to = "party",
               values_to = "pct") %>%
  pivot_longer(starts_with("pvi"),
               names_to = "pvi_party",
               values_to = "pvi") %>%
  mutate(filter_flag = case_when(party == "democratic" & pvi_party == "pvi_3d" ~ "keep",
                                 party == "republican" & pvi_party == "pvi_3r" ~ "keep",
                                 party == "other" & pvi_party == "pvi_3o" ~ "keep",
                                 TRUE ~ "drop")) %>%
  filter(filter_flag == "keep") %>%
  select(-filter_flag) %>%
  select(year, state, party, pct, pvi) %>%
  nest(data = -state) %>%
  slice_sample(n = 6) %>%
  unnest(data) %>%
  ggplot(aes(x = year,
             y = pvi,
             color = party)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 0.25) +
  geom_line() +
  NatParksPalettes::scale_color_natparks_d("Triglav") +
  scale_y_percent() +
  facet_wrap(~state) +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Partisan lean of a select number of states**",
       x = NULL,
       y = NULL) +
  expand_limits(y = c(-0.15, 0.15))

ggquicksave("dev/04-3pvi/3pvi_sample.png")

