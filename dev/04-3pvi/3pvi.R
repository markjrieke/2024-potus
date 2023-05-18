library(tidyverse)

cpvi <- read_csv("data/cpvi.csv")

# national results (d/r/o):
# 20: 0.513 / 0.468 / 0.019
# 16: 0.482 / 0.461 / 0.057

# 3pvi weighting set to 75/25 for 2020/2016

pvi_3 <-
  cpvi %>%
  mutate(other_20 = 1 - dem_20 - rep_20,
         other_16 = 1 - dem_16 - rep_16,
         pvi_3d20 = dem_20 - 0.513,
         pvi_3r20 = rep_20 - 0.468,
         pvi_3o20 = other_20 - 0.019,
         pvi_3d16 = dem_16 - 0.482,
         pvi_3r16 = rep_16 - 0.461,
         pvi_3o16 = other_16 - 0.057,
         pvi_3d = pvi_3d20*0.75 + pvi_3d16*0.25,
         pvi_3r = pvi_3r20*0.75 + pvi_3r16*0.25,
         pvi_3o = pvi_3o20*0.75 + pvi_3o16*0.25) %>%
  select(State,
         CPVI,
         dem_20, rep_20, other_20,
         dem_16, rep_16, other_16,
         pvi_3d,
         pvi_3r,
         pvi_3o)

pvi_3 %>%
  arrange(desc(pvi_3o)) %>%
  select(state = State,
         starts_with("pvi")) %>%
  pivot_longer(starts_with("pvi"),
               names_to = "party",
               values_to = "3pvi") %>%
  mutate(party = str_remove_all(party, "pvi_3"),
         party = case_match(party,
                            "d" ~ "Dem",
                            "r" ~ "Rep",
                            "o" ~ "Other"),
         state = tidytext::reorder_within(state, `3pvi`, party),
         color = case_when(party == "Dem" ~ "royalblue",
                           party == "Rep" ~ "red",
                           party == "Other" ~ "gray40")) %>%
  ggplot(aes(y = state,
             x = `3pvi`,
             color = color)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.25) +
  geom_point(size = 2) +
  scale_color_identity() +
  riekelib::scale_x_percent() +
  tidytext::scale_y_reordered() +
  facet_wrap(~party, scales = "free") +
  riekelib::theme_rieke() +
  labs(title = "**3-PVI: A new metric for measuring partisan lean**",
       x = NULL,
       y = NULL)

riekelib::ggquicksave("dev/04-3pvi/3pvi.png",
                      width = 9,
                      height = 12)
