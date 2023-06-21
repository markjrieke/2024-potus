
# 2024-potus

repo for constructing a 2024 presidential election forecast

### loose methodology

Vaguely, the following:

- Set priors based on time-for-change model in each state & at the
  national level
- ‘Predict’ each poll’s result
- Utilize a poisson likelihood for each candidate (D/R/O)
- simplex of probabilities - use other as the pivot
- each candidate gets their own linear model
- Linear model has offset terms for pollster, mode, population, state,
  and date, plus a region invariant polling bias.
- state offset is set by a gaussian process using mahalonobis distance
  between the states
- date offset is set by a gaussian process over time.
- Parameters for estimating the national vote distribution are the
  population weighted average of the state variables (one less set of
  parameters).

So, vaguely, for each party:

$$
\begin{align*}
\phi_{\text{state, date}} & = \beta_{\text{state, date}} + \beta_{\text{pollster}} + \beta_{\text{mode}} + \beta_{\text{population}} + \beta_{\text{state}} + \beta_{\text{noise}} \\
\beta_{\text{state, date}} & \sim \text{Gaussian process} \dots \\
\beta_{\text{state}} & \sim \text{Gaussian process} \dots
\end{align*}
$$

### loose workflow

- targeted workflow
- FRED data feed into priors
- tidycensus data feed into priors
- FTE pulled in for polling
- Exclusion of Traflagar, Rasmussen, Center Street
- GH action for publishing

### references

- Linzer 2013 paper
- Pierre Kemp 2016 model
- Economist 2020 model
- Abramovitz time-for-change
- FTE
- FRED
- Census

### notes on abramovitz data/nat-3pv

- Incumbent net approval pulled from FiveThirtyEight’s averages on the
  day before the presidential election. If the exact date is not
  available due to data resolution, (these are manually pulled) the net
  approval from the closest day *prior* to election day is used instead.
- Third party flag is set to 1 whenever an individual third party
  candidate garners more than 5% of the national popular vote.
- Abramovitz 2012 mod can be found
  [here](https://www.washingtonpost.com/blogs/ezra-klein/files/2012/08/abramowitz.pdf)
- For Biden’s net approval, pulling the *All Polls* variant of
  [FiveThirtyEight’s presidential approval
  tracker](https://projects.fivethirtyeight.com/biden-approval-rating/?cid=rrpromo)
  (this is consistent with what’s displayed for the previous
  presidents).

### multinomial identification notes

- Ref [this
  post](https://eleafeit.com/posts/2021-05-23-parameterization-of-multinomial-logit-models-in-stan/)
  for some notes on identification in categorical models
- Also, identification for categorical (multinomial) models is covered
  in the stan user guide
  [here](https://mc-stan.org/docs/stan-users-guide/multi-logit.html) and
  [here](https://mc-stan.org/docs/stan-users-guide/parameterizing-centered-vectors.html).

### 3pvi notes

- Cook PVI pulled from [this
  table](https://www.cookpolitical.com/cook-pvi/2022-partisan-voting-index/state-map-and-list).
- 75/25 weighted split between two most recent elections based on Cook’s
  2023 methodology.

### misc notes

- [density database](https://densitydb.github.io/) gives statewide
  population weighted density estimates.
- how others forecast
  - [Economist](https://github.com/TheEconomist/us-potus-model/tree/master)
    — Bayesian logistic regression model
  - [DDHQ](https://forecast.decisiondeskhq.com/methodology) — ensemble
    of ridge, random forest, elastic net, and gradient boosts
  - [Race2WH](https://twitter.com/loganr2wh/status/1575673680364859392)
    — normal approximation of candidate voteshare
  - [JHKForecasts](https://projects.jhkforecasts.com/presidential-forecast/forecast_methodology)
    — simulation methods based on a normal approximation under the
    central limit theorem
  - [Cory McCartran, Data for
    Progress](https://github.com/CoryMcCartan/midterms-22) — Bayesian
    model with a student-t response
  - [FTE](https://fivethirtyeight.com/features/how-fivethirtyeights-2020-presidential-forecast-works-and-whats-different-because-of-covid-19/)
    — dig into once not on the MH network
  - [NYT](https://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html)
    — dig into once not on the MH network

### prior thoughts

Here’s what I’m looking at for the prior linear model for a given party
in a given state:

$$
\begin{align*}
\phi &= \alpha_{\text{national}} + \alpha_{\text{state}} \\
\alpha_{\text{national}} &= \gamma_{\text{political}} + \gamma_{\text{economic}} \\
\gamma_{\text{political}} &= \beta_{\text{inc status}} + (\beta_{\text{approval}} + \beta_{\text{approval} \times \text{inc status}})A + \beta_{\text{third party}} \\
\gamma_{\text{economic}} &= (\beta_{\text{GDP}} + \beta_{\text{GDP} \times \text{inc status}})G \\
\alpha_{\text{state}} &= \beta_{\text{3D}}P_{\text{3D}} + \beta_{\text{3R}}P_{\text{3R}} + \beta_{\text{3O}}P_{\text{3O}}
\end{align*}
$$
