library(readr)

# 2020 FTE polls
read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls_historical.csv") %>%
  write_csv("data/polls/president_2020.csv")

# 2016 Economist polls
read_csv("https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/all_polls.csv") %>%
  write_csv("data/polls/president_2016.csv")

# 2012 Economist polls
read_csv("https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/all_polls_2012.csv") %>%
  write_csv("data/polls/president_2012.csv")

# 2008 Economist polls
read_csv("https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/all_polls_2008.csv") %>%
  write_csv("data/polls/president_2008.csv")

# 2016 FTE polls
# read_csv("http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv") %>%
#   write_csv("data/polls/president_2016.csv")
