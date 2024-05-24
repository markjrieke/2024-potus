# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)

# functions
walk(list.files("R/model/functions/"), ~source(paste0("R/model/functions/", .x)))

# create stan model exe directory
if (!dir.exists("exe")) {
  dir.create("exe")
}

# pull in any new days to run

# approval prior model ---------------------------------------------------------

run_approval_prior_model()

# approval model ---------------------------------------------------------------

# pvi model --------------------------------------------------------------------

# prior model ------------------------------------------------------------------

# poll model -------------------------------------------------------------------
