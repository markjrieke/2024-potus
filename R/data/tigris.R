library(tidyverse)

tigris::states(class = "sf", cb = TRUE) %>%
  tigris::shift_geometry() %>%
  filter(GEOID < 60) %>%
  write_rds("data/tigris/tigris.rds")
