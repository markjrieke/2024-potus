
# 2024-potus

repo for constructing a 2024 presidential election forecast

### loose methodology

See [associated notion
timeline](https://www.notion.so/rafrieke/2024-Presidential-Election-90855891b84345e69edad0151ec02bdf)
for more detail, but model is loosely the following:

$$
\begin{align*}
\text{Y}_i &\sim \text{Binomial}(\text{K}_i,\ \theta_i) \\
\text{logit}(\theta) &= \beta_s + \beta_{s,d} + \beta_p + \beta_m + \beta_g + \beta_c + \beta_n \\
\end{align*}
$$

$\beta_s$ and $\beta_{s,d}$ comprise the “true” latent voting intention
in each state. $\beta_s$ is the time-invariant component, set by a
Gaussian process over the euclidean distance between states in some
normalized feature space and $\beta_{s,d}$ is a the daily time-varying
offset from each state’s time-invariant component, set by a AR(1) /
Gaussian random walk process over time in each state. The predicted
voteshare in state $s$ on day $d=E$ (election day) is
$\text{expit}(\beta_s + \beta_{s,E})$. The state-level prior is either
over the time-invariant parameter *or* the predicted voteshare (TBD).

The remaining parameters account for bias in the individual polls:

- $\beta_p$: pollster
- $\beta_m$: poll mode (online, RDD, etc.)
- $\beta_g$: poll group (RV, LV, adults, etc.)
- $\beta_c$: candidate/party sponsor (D, R, or none)
- $\beta_n$: noise (per poll!)

Most of these have sufficient groups to be modeled hierarchically. I may
model $\beta_g$ and $\beta_c$ with fixed effects, given the small number
of groups in these parameters.

### resources

- Models & methodology
  - [Linzer 2013
    paper](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf)
  - [Pierre Kemp 2016
    model](https://www.slate.com/features/pkremp_forecast/report.html)
  - [Economist 2020
    model](https://github.com/TheEconomist/us-potus-model?tab=readme-ov-file)
  - [Abramovitz
    time-for-change](https://www.washingtonpost.com/blogs/ezra-klein/files/2012/08/abramowitz.pdf)
  - [FTE
    2020](https://projects.fivethirtyeight.com/2020-election-forecast/)
  - [FTE
    2016](https://projects.fivethirtyeight.com/2016-election-forecast/)
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
  - [Gelman/Microsoft](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/04/forecasting-with-nonrepresentative-polls.pdf)
  - [FTE](https://fivethirtyeight.com/features/how-fivethirtyeights-2020-presidential-forecast-works-and-whats-different-because-of-covid-19/)
    — dig into once not on the MH network
  - [NYT](https://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html)
    — dig into once not on the MH network
- Data
  - [FRED](https://fred.stlouisfed.org/)
  - [FTE
    Polls](https://github.com/fivethirtyeight/data/tree/master/polls)
  - [Urban Stats](https://urbanstats.org/)
  - [Cook](https://www.cookpolitical.com/cook-pvi)
- Misc
  - [FTE poll inclusion
    policy](https://abcnews.go.com/538/538s-polls-policy-faqs/story?id=104489193)
  - [FTE weighting
    methodology](https://abcnews.go.com/538/polling-averages-work/story?id=104546697)
  - [Notes on
    copulas](https://www.pymc.io/projects/examples/en/latest/howto/copula-estimation.html)
    (used for generating distances in some feature space)

### banned pollsters

- [Center Street
  PAC](https://gelliottmorris.substack.com/p/the-gory-details-about-how-modern)
- [Traflagar](https://split-ticket.org/2022/09/19/whats-going-on-with-trafalgars-polls/)
- [Rasmussen](https://web.archive.org/web/20240308212818/https://www.washingtonpost.com/politics/2024/03/08/rasmussen-538-polling/)
- Any pollsters who [use LLMs to generate
  results](https://ash.harvard.edu/articles/using-ai-for-political-polling/)
  will be banned.

### loose workflow

- derived data (constant)
  - distance matrices
  - cpvi
- (approval model?) \[may not actually do, we’ll see…\]
  - approval data
  - e-day approval model
  - write results
  - write diagnostics
- prior model
  - economic data
  - approval data (or model)
  - fit
  - state-level priors
  - write results
  - write diagnostics
- poll model
  - polling data
  - prior data
  - fit
  - write results
  - write diagnostics
- reporting
  - update site
  - blastula email diagnostics

### implementation notes

- See
  [here](https://gist.github.com/dgkeyes/0ea5f587b8cfb2b8f12f7be06c77c52b)
  for an example of y-axis text inlay using the
  [ggh4x](https://teunbrand.github.io/ggh4x/index.html) package
- See [here](https://rfortherestofus.com/2023/05/github-actions) for a
  basic overview of automating scripts on a schedule with github
  actions.
- See
  [here](https://stackoverflow.com/questions/76642914/use-a-github-package-for-a-r-github-action)
  for an example of installing a package from github as a part of a
  github action

### misc notes

- Colors for display!
  - Safe D (\>99): 3579AC
  - Very Likely D (99 \>= x \> 85): 7CB0D7
  - Likely D (85 \>= x \> 65): D3E5F2
  - Uncertain (65 \>= x \<= 65): F2F2F2
  - Likely R (65 \< x \<= 85): F2D5D5
  - Very Likely R (85 \< x \<= 99): D78080
  - Safe R (\>99): B13737
- abramovitz data notes
  - Incumbent net approval pulled from FiveThirtyEight’s averages on the
    day before the presidential election. If the exact date is not
    available due to data resolution, (these are manually pulled) the
    net approval from the closest day *prior* to election day is used
    instead.
  - Third party flag is set to 1 whenever an individual third party
    candidate garners more than 5% of the national popular vote.
  - For Biden’s net approval, pulling the *All Polls* variant of
    [FiveThirtyEight’s presidential approval
    tracker](https://projects.fivethirtyeight.com/biden-approval-rating/?cid=rrpromo)
    (this is consistent with what’s displayed for the previous
    presidents).
