
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
  and date
- state offset is set by a gaussian process using mahalonobis distance
  between the states
- date offset is set by a gaussian process over time

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
