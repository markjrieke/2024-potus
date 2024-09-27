# README

This directory contains the output of each of the models in the pipeline. Each folder contains the output of the corresponding model found in the `stan/` directory under root. `model_log.csv` is a running log that is appended after each model run and contains diagnostic information, versioning, and run times. 

## approval/

* `e_day_approval_current.csv`: Biden's forecasted net approval on election day.

## approval-prior/

* `delta_prior.csv`: How much an incumbent president's net approval tends to change over the course of the election cycle (189 days prior to election).
* `e_day_approval_historical.csv`: The estimated election day net approval for incumbent presidents from 1948 onwards.

## polls/

* `evs.csv`: Median estimate and 66/95% credible intervals for the forecasted number of electoral college votes won by Harris.
* `polls_out.csv`: A dataframe of formatted polls that feed into the poll model.
* `polls_raw.csv`: A dataframe of raw polling data extracted from FiveThirtyEight's [polling database](https://github.com/fivethirtyeight/data/tree/master/polls).
* `theta.csv`: Median estimate and 66/95% credible intervals for Harris' forecasted two-party voteshare in each state.
* `tie_pres.csv`: The forecasted probability of a tie in the electoral college.
* `win_pres.csv`: Harris' forecasted probability of winning the electoral college.
* `win_state.csv`: Harris' forecasted probability of winning in each state.
* `conditional_probabilities.csv`: Harris' forecasted probability of winning the electoral college conditional on having won/lost in each state.
* `beta_b.csv`: State-level polling bias.
* `beta_c.csv`: Sponsor-party polling bias.
* `beta_g.csv`: Group (population) polling bias.
* `beta_m.csv`: Mode polling bias.
* `beta_p.csv`: Pollster bias.

## priors/

* `priors.csv`: State-level priors for both Biden and Harris, updated daily. Priors are given on the [logit scale](https://en.wikipedia.org/wiki/Logit).

## pvi/

* `pvi_summary.csv`: An estimate of each state's partisan lean, on the natural (probability) scale.
