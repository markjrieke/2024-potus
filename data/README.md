# README

This directory contains static data entered by hand as well as pre-computed data that does not need to be re-evaluated upon each model run. The data generating scripts can be found in the `R/data/` folder under root. 

## approval/

* `{last_name}.html`: raw html files of prior presidents' net approval ratings extracted from [FiveThirtyEight](https://projects.fivethirtyeight.com/biden-approval-rating/).
* `historical_approval.csv`: a table of net approval per day of presidency per president as extracted by `R/data/approval.R`.

## features/

For more information on how this data is generated, see the README under the `data/` folder under root.

* `F_a.rds`: a matrix measuring the distance between states in the feature space. Includes both "raw" and "aggregate" states.
* `F_r.rds`: a matrix measuring the distance between states in the feature space. Only includes "raw" states. States that are an aggregate of subgroups (i.e., Nebraska and Maine, in which candidates are allocated electoral college votes by state and congressional district). 
* `F_s.rds`: a matrix measuring the similarity between states. Similarity is defined as 1 - distance.
* `sid.csv`: a table mapping each state to an integer id. Includes both "raw" and aggregate states.
* `wt.rds`: a matrix with the population weight each state contributes to each aggregate state.

## polls/

Contains historical polls from previous election cycles for backtesting.

* `president_2008.csv`
* `president_2012.csv`
* `president_2016.csv`
* `president_2020.csv`

## static/

Contains hand-entered datasets. Sources are listed below.

* `abramowitz.csv`: initially used as source data for the prior model, contains basic information on elections since 1948 regarding incumbency, the incumbent party, incumbent approval, and the result.
* `allowed_candidates.csv`: candidates who the model allows to appear on polls that feed into the model. If a candidate not on this list appears in a poll, that poll is not used to model the outcome (e.g., polls that include, say, Michelle Obama are not included since Obama does not represent a candidate voters can actually vote for come November).
* `banned_pollsters.csv`: contains pollsters whose polls are not allowed to contribute to the model. Banned pollsters include:
  * [Center Street PAC](https://gelliottmorris.substack.com/p/the-gory-details-about-how-modern): for utilizing a poor sampling and weighting strategy that severely underrepresents the uncertainty in their polls and severely overestimates Democratic support.
  * [Traflagar](https://split-ticket.org/2022/09/19/whats-going-on-with-trafalgars-polls/): for utilizing an opaque sampling methodology that appears to involve repeatedly contact voters from a consistent pool (rather than reaching out to new respondents until the desired number of responses is met).
  * [Rasmussen](https://web.archive.org/web/20240308212818/https://www.washingtonpost.com/politics/2024/03/08/rasmussen-538-polling/): for refusing to provide basic methodological information to FiveThirtyEight and consistently engaging in spreading conspiracy theories and false information as a polling outlet.
  * Any pollsters who [use LLMs to generate results](https://ash.harvard.edu/articles/using-ai-for-political-polling/) will be banned.
* `cpvi.csv`: results from the previous two elections and associated CPVI, taken from [Cook](https://www.cookpolitical.com/cook-pvi).
* `electors.csv`: contains the number of electors per state.
* `population_rank.csv`: rank of preferred voting populations. Used to avoid overcounting responses. When a single poll includes responses from multiple populations, the lowest rank (most preferred) population responses are used.
* `state_centers.csv`: longitude and latitude used to place points representing the number of electors on the electoral college map.
* `statewide_results.csv`: historical results in each state for each election from 1948 to 2020.
* `urban_stats.csv`: contains features about each state (demographic, economic, etc.).

## tigris/

* `tigris.rds`: sf data exported from the `{tigris}` package.
