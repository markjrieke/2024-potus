
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
