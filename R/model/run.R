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

# time invariant models --------------------------------------------------------

run_approval_prior_model()
run_pvi_model()

# approval model ---------------------------------------------------------------

# prior model ------------------------------------------------------------------

# poll model -------------------------------------------------------------------
