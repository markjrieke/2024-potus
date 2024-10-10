
# 2024 Presidential Forecast

This repository contains the code for running a dynamic and hierarchical
Bayesian model that forecasts election outcomes in states, the nation,
and the electoral college. The model is written in
[Stan](https://mc-stan.org/) and the supporting pipeline is written in
[R](https://www.r-project.org/).

The model improves upon the [Economist’s 2020
model](https://github.com/TheEconomist/us-potus-model) (which, in turn,
improved upon [Pierre Kemp’s
implementation](https://www.slate.com/features/pkremp_forecast/report.html)
of [Drew Linzer’s
model](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf))
by estimating the parameters used to generate state covariance matrices,
rather than being passed the matrices as data. I intend to write a
formal explanation of the model, likely after the election has
concluded. In the interim, you can view a brief overview of the model
definition in the [README in the `stan/`
folder](https://github.com/markjrieke/2024-potus/tree/main/stan).

A more general overview of the model methodology can be found
[here](https://www.thedatadiary.net/posts/2024-07-04-forecast-methodology/),
and the full output can be explored
[here](https://www.thedatadiary.net/2024-potus/national).

## Version history

### 2.11

###### 2024-10-10

- Added a paramter, $\beta_i$, to model the effect of internal polls.
- $\beta_i$ is separate from $\beta_c$, which models the effect of
  party-sponsored polls.

### 2.10

###### 2024-09-28

- Use `pollster_rating_name` rather than `pollster` to uniquely identify
  pollsters.
- This coalesces pollsters who appear under multiple name into one
  (e.g., *HarrisX* and *HarrisX/Harris Poll* now both fall under *Harris
  Insights & Analytics*, rather than being considered separate
  pollsters).
- This affects the following pollsters:
  - **SurveyUSA**: previously *SurveyUSA* or *SurveyUSA/High Point
    University*
  - **Quantus Insights**: previously *Quantus Insights* or *Quantus
    Polls and News*
  - **YouGov**: previously *YouGov* or *YouGov Blue*
  - **Harris Insights & Analytics**: previously *HarrisX* or
    *HarrisX/Harris Poll*
  - **Change Research**: previously *Change Research* or *Embold
    Research*
  - **SoCal Research**: previously *SoCal Research* or *SoCal
    Strategies*
  - **Fabrizio, Lee & Associates**: previously *Fabrizio* or *Fabrizio
    Ward*
  - **The Tyson Group**: previously *The Tyson Group* or *P2 Insights*

### 2.9

###### 2024-09-27

- Parameter outputs for the poll model are now saved to the `out/polls/`
  directory:
  - beta_b: state-level polling bias
  - beta_c: party-sponsor polling bias
  - beta_g: group (population) polling bias
  - beta_m: mode polling bias
  - beta_p: pollster bias

### 2.8

###### 2024-09-26

- Included Shiva Ayyadurai and Lars Mapstead as [allowed
  candidates](https://ballotpedia.org/Presidential_candidates,_2024).
  Polls including them will now be included in the model.

### 2.7

###### 2024-09-25

- Included Joseph Kishore as an [allowed
  candidate](https://ballotpedia.org/Presidential_candidates,_2024).
  Polls including him will now be included in the model.

### 2.6

###### 2024-09-18

- Included Claudia De la Cruz as an [allowed
  candidate](https://ballotpedia.org/Presidential_candidates,_2024).
  Polls including her will now be included in the model.

### 2.5

###### 2024-09-12

- Updated state and national headline text to refer to raw probability,
  rather than ratings.
- Removed rating textbar above state map.
- Set state bubble color and summary bar fill based on a continuous
  gradient scaled by candidate probability of winning.
- Added linked interactivity between map and bar.
- Updated css to `cursor:pointer` when hovering over the map/bar to more
  intuitively suggest that users can click and be taken to the state
  pages.

### 2.4

###### 2024-09-03

- Added ActiVote to the [banned pollsters
  list](https://github.com/markjrieke/2024-potus/blob/main/data/static/banned_pollsters.csv).

### 2.3

###### 2024-08-19

- Added a filter to drop poll questions with a `NA` sample size.
  - This was previously an implicit drop.
  - If a poll included multiple populations, but had incomplete sample
    size data for the [best
    population](https://github.com/markjrieke/2024-potus/blob/main/data/static/population_rank.csv),
    the entire poll could have been dropped.
  - This ensures that the poll is included provided at least one of the
    populations has sample size data.

### 2.2

###### 2024-08-12

- Added a model review/diagnostic display under `out/REVIEW.md`

### 2.1

###### 2024-08-11

- Added conditional probability plot functions for the National page.

### 2.0

###### 2024-08-03

#### Harris Model

- Modified the prior/polling stan models and supporting R pipelines to
  support Kamala Harris as the democratic candidate. More details can be
  found in the `stan/` directory README.
- Updated site functions’ internal variable naming and public facing
  candidate names to refer to Kamala Harris.
- Updated time-series plots on site to only display projections from 8/1
  onwards.
- Updated function documentation and READMEs based on new output.
- Fully re-ran the entire model from 5/1 onwards. An archived version of
  the final run with Biden as the democratic candidate can be found in
  the
  [`archive-biden`](https://github.com/markjrieke/2024-potus/tree/archive-biden)
  branch in this repository.

#### Other Fixes

- Corrected the need to run the polling model with the `prior_check`
  flag for run dates before 5/7.
  ([\#10](https://github.com/markjrieke/2024-potus/issues/10))
- `render_interactive_map()` now correctly renders the ggobj passed as
  an argument, rather than looking for a specific object in the global
  environment.
- Fixed a small bug causing the prior stan model to underestimate
  uncertainty in the combination of the estimated national voteshare and
  state partisan lean.

### 1.2

###### 2024-07-16

- [Added conditional probability as a poll model
  output](https://github.com/markjrieke/2024-potus/commit/6eee3a3a8d08b6df66f71c3961e049fd91494c79).
- The output is not yet made available in the UI, but will be as a
  result of a later release.

### 1.1

###### 2024-07-14

- Modified header text to include the article “the” on D.C.’s state
  page.

### 1.0

###### 2024-07-04

- Initial release

## Other forecasts

- [FiveThirtyEight](https://projects.fivethirtyeight.com/2024-election-forecast/)
- [The
  Economist](https://www.economist.com/interactive/us-2024-election/prediction-model/president/)
- [JHKForecasts](https://projects.jhkforecasts.com/2024/president/#standard)
- [DDHQ](https://elections2024.thehill.com/forecast/2024/president/)
- [Silver
  Bulletin](https://www.natesilver.net/p/nate-silver-2024-president-election-polls-model)
  (requires subscription to view output)
