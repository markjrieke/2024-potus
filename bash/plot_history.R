library(tidyverse)
library(riekelib)

# read in commit history
history <- read_lines("bash/history.log")

# plot history
tibble(history = history) %>%
  mutate(commit = if_else(str_sub(history, 1, 7) == "commit ", history, NA_character_)) %>%
  fill(commit) %>%
  nest(commit_data = -commit) %>%
  mutate(author = map_chr(commit_data, ~.x[2,1]$history),
         time = map_chr(commit_data, ~.x[3,1]$history),
         message = map_chr(commit_data, ~.x[5,1]$history),
         commit_data = map(commit_data, rowid_to_column)) %>%
  nplyr::nest_filter(commit_data, rowid >= 7) %>%
  nplyr::nest_filter(commit_data, history != "") %>%
  nplyr::nest_select(commit_data, -rowid) %>%
  unnest(commit_data) %>%
  mutate(commit = str_remove_all(commit, "commit "),
         author = str_remove_all(author, "Author: "),
         author = str_remove_all(author, " <.*?>"),
         time = str_remove_all(time, "Date:   "),
         time = str_sub(time, 5),
         year = as.integer(str_sub(time, -4)),
         month = map_int(time, ~which(month.abb == str_sub(.x, 1, 3))),
         day = as.integer(str_sub(time, 5, -15)),
         date = paste(year, month, day, sep = "-"),
         datetime = paste(date, str_sub(time, -13, -6)),
         datetime = ymd_hms(datetime),
         message = str_sub(message, 5)) %>%
  separate(history, c("additions", "deletions", "file"), "\\\t") %>%
  mutate(across(c(additions, deletions), as.integer),
         across(c(additions, deletions), ~replace_na(.x, 0))) %>%
  mutate(extension = str_sub(file, str_locate(file, "\\.")[,1] + 1, -1)) %>%
  select(commit,
         author,
         message,
         datetime,
         file,
         extension,
         additions,
         deletions) %>%
  mutate(extension = if_else(str_detect(extension, "=>"),
                             str_sub(extension, 1, str_locate(extension, "=")[,1] - 2),
                             extension)) %>%
  # mutate(extension = if_else(extension == "R => imports.R}", "R", extension)) %>%
  # filter(!extension %in% c("csv", "exe", "png", "html")) %>%
  filter(extension %in% c("R", "stan")) %>%
  group_by(commit, extension, message, datetime) %>%
  summarise(additions = sum(additions),
            deletions = sum(deletions)) %>%
  ungroup() %>%
  arrange(datetime) %>%
  group_by(extension) %>%
  mutate(loc = cumsum(additions) - cumsum(deletions)) %>%
  ggplot(aes(x = datetime,
             y = loc,
             color = extension)) +
  geom_step() +
  scale_y_comma() +
  theme_rieke()





