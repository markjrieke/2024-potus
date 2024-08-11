# README

This directory contains all the R scripts and functions necessary to setup static data, run the modeling pipeline, and support forecast pages online. Individual functions are documented in detail, but a general overview for each file can be found here.

## data/

Contains scripts for generating data that doesn't need to be recomputed each run but needs to be derived programatically. The outputs of each script live in the `data/` directory under root. 

* `approval.R`
  * Converts raw html graphs of historical net presidential approval to a dataframe of net approval per day of presidency per president. 
  * Raw data is extracted from FiveThirtyEight's [presidential approval tracker](https://projects.fivethirtyeight.com/biden-approval-rating/).
* `features.R`
  * Generates a series of outputs measuring the [euclidean distance](https://en.wikipedia.org/wiki/Euclidean_distance) between states in a feature space.
  * State-level feature information are sourced from [Urban Stats](https://urbanstats.org/) and include:
    * Log of the population-weighted density
    * Proportion of population that is white, black, hispanic, and asian
    * Proportion of population that are citizens by birth
    * An index measuring household income in the state
    * An index measuring educational attainment in the state
  * Features are standardized such that the scale of a feature doesn't dominate the distance estimation. Features on the interval `[0, 1]` are re-cast to the unconstrained interval via a [copula transformation](https://www.pymc.io/projects/examples/en/latest/howto/copula-estimation.html) prior to standardization so that the standardized feature is approximately normally distributed. 
  * The resulting distance matrix between states is scaled such that the maximum distance between any two states is `1`.
  * The following outputs are created:
    * `F_r.rds`: a matrix measuring the distance between states in the feature space. Only includes "raw" states. States that are an aggregate of subgroups (i.e., Nebraska and Maine, in which candidates are allocated electoral college votes by state and congressional district). 
    * `F_a.rds`: a matrix measuring the distance between states in the feature space. Includes both "raw" and "aggregate" states.
    * `F_s.rds`: a matrix measuring the similarity between states. Similarity is defined as 1 - distance.
    * `sid.csv`: a table mapping each state to an integer id. Includes both "raw" and aggregate states.
    * `wt.rds`: a matrix with the population weight each state contributes to each aggregate state.
* `img.R`
  * Generates state similarity plots based on the feature matrices generated in `features.R`.
  * Rendering similarity plots locally save a bit of rendering time on site deployment.
* `polls.R`
  * Imports and saves polls collected in prior election cycles (2008-2020) for backtesting.
  * Imported from [FiveThirtyEight](https://projects.fivethirtyeight.com/polls/) and [The Economist](https://github.com/TheEconomist/us-potus-model/tree/master/data).
* `tigris.R`
  * Imports a shapefile of the US from the `{tigris}` package and saves locally.
  * Saves a bit of rendering time.

## model/

Contains functions for running each of the pipelines for each model as well as a over-arching `run.R` script to run the entire pipeline. Models themselves are in the `stan/` directory under root. Model outputs are contained in the `out/` directory under root. 

* `functions/`
  * `approval-prior.R`: runs the data pipeline/model for the approval-prior model. Generates estimates for election-day net approval for all incumbent presidents from 1948-2020, as well as an estimate of how much net approval tends to change throughout the election cycle. Only rerun when any of the upstream dependencies are changed.
  * `approval.R`: runs the data pipeline/model for the approval model. Generates an estimate for Biden's net approval on election day. Rerun every day using new data from FiveThirtyEight's [approval tracker](https://projects.fivethirtyeight.com/biden-approval-rating/).
  * `polls.R` runs the data pipeline/model for the poll model. Rerun every day using new data from FiveThirtyEight's [polling database](https://github.com/fivethirtyeight/data/tree/master/polls). Generates the following estimates:
    * Median, 66, and 95% credible interval estimates for the forecasted number of elecotral college votes won by Harris each day.
    * A formatted table of polls.
    * An unformatted table of raw polling data as extracted from FiveThirtyEight.
    * Median, 66, and 95% credible interval estimates for the forecasted two-party voteshare that Harris wins in each state on each day.
    * The forecasted probability of a tie in the electoral college on each day.
    * The forecasted probability of Harris winning the electoral college on each day.
    * The forecasted probability of Harris winning in each state on each day.
    * The forecasted probability of Harris winning the electoral college conditional on having won/lost in each state.
  * `priors.R`: runs the data pipeline/model for the prior model. Generates state-level election day priors for both Biden and Harris. Rerun every day using updated approval model output. 
  * `pvi.R`: runs the data pipeline/model for the pvi (partisan voter index) model. Generates estimates of each state's partisan lean based on Cook's [CPVI](https://www.cookpolitical.com/cook-pvi). Only rerun when any of the upstream dependencies are changed.
  * `utils.R`: contains common functions used throughout the pipeline.
* `run.R`: runs the entire pipeline and any out-of-date models.

## site/

* `plot-conditionals.R` generates conditional probability plots for each candidate.
* `plot-evs.R`: generates the forecasted electoral college outcome plot.
* `plot-map.R`: generates the current state ratings map.
* `plot-prob.R`: generates state or national level probability of winning plots.
* `plot-utils.R`: common functions used throughout other plotting functions.
* `plot-voteshare.R`: generates state or national level forecasted voteshare plots.
* `setup.R`: sets up libraries and environment variables needed to render site pages.
* `table-polls.R`: generates state or national polls tables.
* `text-dynamic.R`: generates formatted text that updates based on the state of the race.
* `text-static.R`: generates formatted static text.
* `utils.R`: common functions used throughout the site.

