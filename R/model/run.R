# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(riekelib)

# functions
walk(list.files("R/model/functions/"), ~source(paste0("R/model/functions/", .x)))

# create stan model exe directory
if (!dir.exists("exe")) {
  dir.create("exe")
}

# pull in any new days to run

# time invariant models --------------------------------------------------------

run_approval_prior_model()
run_pvi_model()

# daily models -----------------------------------------------------------------

missing_days("out/approval/e_day_approval_current.csv") %>%
  walk(run_approval_model)

missing_days("out/priors/priors.csv") %>%
  walk(run_prior_model)

# prior model ------------------------------------------------------------------

# poll model -------------------------------------------------------------------
