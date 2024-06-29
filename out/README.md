# README

This directory contains the output of each of the models in the pipeline. Each folder contains the output of the corresponding model found in the `stan/` directory under root. `model_log.csv` is a running log that is appended after each model run and contains diagnostic information, versioning, and run times. 

## approval/

* `e_day_approval_current.csv`: Biden's forecasted net approval on election day.

## approval-prior/

* `delta_prior.csv`: How much an incumbent president's net approval tends to change over the course of the election cycle (189 days prior to election).
* `e_day_approval_historical.csv`: The estimated election day net approval for incumbent presidents from 1948 onwards.

## polls/

* `evs.csv`: Median estimate and 66/95% credible intervals for the forecasted number of electoral college votes won by Biden.
* `polls_out.csv`: A dataframe of formatted polls that feed into the poll model.
* `polls_raw.csv`: A dataframe of raw polling data extracted from FiveThirtyEight's [polling database](https://github.com/fivethirtyeight/data/tree/master/polls).
* `theta.csv`: Median estimate and 66/95% credible intervals for Biden's forecasted two-party voteshare in each state.
* `tie_pres.csv`: The forecasted probability of a tie in the electoral college.
* `win_pres.csv`: Biden's forecasted probability of winning the electoral college.
* `win_state.csv`: Biden's forecasted probability of winning in each state.

## priors/

* `priors.csv`: State-level priors, updated daily. Priors are given on the [logit scale](https://en.wikipedia.org/wiki/Logit).

## pvi/

* `pvi_summary.csv`: An estimate of each state's partisan lean, on the natural (probability) scale.
