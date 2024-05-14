library(tidyverse)
library(rvest)

# get a tibble of historical approval per day given a svg
get_approval <- function(file) {

  # fixed translation settings
  y_pixel <- c(77.5, 40)
  y_net <- c(0, 50)

  x_pixel <- c(67.376953125, 136.376953125)
  x_day <- c(514, 1462)

  out <-
    read_html(paste0("data/approval/", file)) %>%
    html_elements("svg") %>%
    html_children() %>%
    pluck(3) %>%
    html_elements(".answer.past") %>%
    pluck(1) %>%
    html_attrs() %>%
    pluck("d") %>%
    str_split(",") %>%
    tibble(d = .) %>%
    unnest(d) %>%
    mutate(splits = str_split(d, "M|L"),
           net = map_chr(splits, ~.x[1]),
           day = map_chr(splits, ~.x[2]),
           across(c(net, day), as.numeric),
           day = (day - x_pixel[1])/(x_pixel[2] - x_pixel[1]) * (x_day[2] - x_day[1]) + x_day[1] + 193,
           day = round(day),
           net = (net - y_pixel[1])/(y_pixel[2] - y_pixel[1]) * (y_net[2] - y_net[1]) + y_net[1]) %>%
    drop_na() %>%
    arrange(day) %>%
    mutate(check = case_when(day == 0 ~ net,
                             net == lag(net) ~ NA,
                             .default = net)) %>%
    drop_na() %>%
    select(day, net)

  return(out)

}

tibble(president = list.files("data/approval/")) %>%
  mutate(data = map(president, get_approval),
         president = str_remove(president, "\\.html")) %>%
  unnest(data) %>%
  write_csv("data/approval/historical_approval.csv")

