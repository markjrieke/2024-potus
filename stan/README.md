# README

This directory contains the stan models used to generate the forecast. All models are run for 10,000 iterations (1,250 iterations across 8 chains). Model outputs can be inputs to other models and must be run sequentially --- see `R/models/run.R` for the ordering.

## approval-prior.stan

The approval prior model utilizes a [dynamic linear model](https://mc-stan.org/docs/functions-reference/distributions_over_unbounded_vectors.html#gaussian-dynamic-linear-models) to estimate the net approval for each incumbent president on each day of the election cycle leading up to election day for elections from 1948 onwards. For a given election cycle, $c$, and day, $d$, the incumbent's net approval, $Y_{c,d}$, is modeled as:

$$
\begin{align*}
Y_{c,d} &\sim \text{Normal}(\theta_{c,d}, \sigma_o)
\end{align*}
$$

$\theta_{c,d}$ is the latent state of net incumbent support on day $d$ of cycle $c$ and $\sigma_o$ is a measure of observational noise. $\theta_{c,d}$ is the result of a random walk:

$$
\begin{align*}
\theta_{c,d} &= \sum_1^d \eta_{c,d} \sigma_s
\end{align*}
$$

$\eta_{c,d}$ is the daily change in net approval in the model's state-space and $\sigma_s$ is the state noise.

The approval prior model generates two outputs:

* $\delta$: a measure of how much presidential net approval can change over the cource of the election cycle. Taken as a random draw from a normal distribution whose mean and standard deviation are defined in terms of the difference between the election day net approval and starting net approval (i.e., $\theta_{c,D} - \theta_{c,1}$).
* $\theta_{c,D}$: the incumbent net approval on election day for all elections from 1948 onwards.

## approval.stan

The approval model utilizes a [dynamic linear model](https://mc-stan.org/docs/functions-reference/distributions_over_unbounded_vectors.html#gaussian-dynamic-linear-models) to estimate Biden's net approval on each day of the election cycle leading up to election day. The math is similar to that of the approval prior model, with the exception that only one cycle is being modeled. On any given day, $d$, Biden's net approval, $Y_d$ is modeled as:

$$
\begin{align*}
Y_d &\sim \text{Normal}(\theta_d, \sigma_o)
\end{align*}
$$

$\theta_d$ is the latent state of net support for Biden on day $d$ and $\sigma_o$ is a measure of observational noise. $\theta_d$ is the result of a random walk:

$$
\begin{align*}
\theta_d &= \sum_1^d \eta_d \sigma_s
\end{align*}
$$

$\eta_d$ is the daily change in net approval in the model's state space and $\sigma_s$ is the state noise. For forecasting election day net approval, a prior (informed by the $\delta$ output of the approval prior model) is given to $\theta_D$:

$$
\begin{align*}
\theta_D &\sim \text{Normal}(\mu_{\theta_D}, \sigma_{\theta_D})
\end{align*}
$$

$\mu_{\theta_D}$ is the sum of Biden's net approval on day $d=1$ and the mean of $\delta$ output from the approval prior model. $\sigma_{\theta_D}$ is the standard deviation output by the approval prior model.

The approval model generates one output: $\theta_D$, Biden's forecasted net approval on election day.

## pvi.stan

The pvi model is a simple linear model that estimates each state's actual partisan lean relative to the nation given the state's [partisan voter index](https://www.cookpolitical.com/cook-pvi), a measure of partisan lean based on the state's voting patterns in the prior two presidential elections. For each state, $s$, in each election cycle $c$, the estimated partisan lean of the state, $Y_{c,s}$ is given as:

$$
\begin{align*}
Y_{c,s} &\sim \text{Normal}(\mu_{c,s}, \sigma) \\
\mu_{c,s} &= \alpha + \beta \times x_{c,s}
\end{align*}
$$

The pvi model generates one output: $Y_s$, the current estimated partisan lean in each state.

## priors.stan

The prior model is based off of Dr. Alan Abramowitz' *[Time for Change](https://www.washingtonpost.com/blogs/ezra-klein/files/2012/08/abramowitz.pdf)* model, which, for each election cycle, $c$, estimates the incumbent party's national two-party voteshare, $V_c$, using net approval, $A_c$, real year-over-year GDP growth in the second quarter of the election year, $G_c$, and whether or not the incumbent is running, $I_c$:

$$
\begin{align*}
V_c &\sim \text{Normal}(\mu_c, \sigma) \\
\text{logit}(\mu_c) &= \alpha + \beta_A A_c + \beta_G G_c + \beta_I I_c 
\end{align*}
$$

Priors for each state, $s$, are given as a draw from a normal distribution where the mean is the national prior, $V$, plus the state partisan lean from the pvi model and the standard deviation is the output of the pvi model.

## polls.stan

The polling model is generates the main results of the forecast. It accomplishes this via a [dynamic linear model](https://mc-stan.org/docs/functions-reference/distributions_over_unbounded_vectors.html#gaussian-dynamic-linear-models) of Biden's support in each state and hierarchical adjustments for biases in the polls, explained further below. For a poll $i$ conducted in state $s$ on day $d$ of the campaign, the number of respondents saying they plan to vote for Biden in November is given as a draw from a binomial distribution:

$$
\begin{align*}
Y_{i,s,d} &\sim \text{Binomial}(K_i, \theta_{i,s,d})
\end{align*}
$$

$K_i$ is the total number of responses in favor of either Biden or Trump in the poll and $\theta_{i,s,d}$ is the modeled proportion of Biden's support in state $s$ on day $d$ with adjustments for polling bias in poll $i$. $\theta_{i,s,d}$ is represented as $\mu_{i,s,d}$ on the logit scale and can be considered as the sum of "true" latent support for Biden, $\gamma$ and bias in the polls, $\delta$.

$$
\begin{align*}
\theta_{i,s,d} &= \text{logit}(\mu_{i,s,d}) \\
\mu_{i,s,d} &= \gamma_{s,d} + \delta_{i,s}
\end{align*}
$$

True support for Biden in state $s$ on day $d$ is the linear combination of the prior mean support, $\alpha_s$, modeled deviation from the prior, $\beta_s$, and a time-varying parameter, $beta_{s,d}$, to capture changes in support throughout the election cycle. 

$$
\begin{align*}
\gamma_{s,d} &= \alpha_s + \beta_s + \beta_{s,d}
\end{align*}
$$

The state level parameters here include "composite" groups that are comprised of subgroups (i.e., Maine and Nebraska, which are composite groups of the congressional districts, and the nation as a whole, which is a composite of all subgroups) and "raw" states that are used to build the composite groups. Each state parameter is then the dot product between the vector of raw state parameters and a vector of weights, $w_s$.

$$
\begin{align*}
\alpha_s &= \alpha_r \cdot w_s \\
\beta_s &= \beta_r \cdot w_s \\
\beta_{s,d} &= \beta_{r,d} \cdot w_s
\end{align*}
$$

The raw state parameter, $\beta_r$, allows for correlation across states and as such is modeled as a draw from a multivariate normal distribution with covariance $K_r$. $K_r$ is estimated using a [exponentiated quadratic kernel](https://mc-stan.org/docs/functions-reference/matrix_operations.html#exponentiated-quadratic-kernel) given a matrix measuring the distance in feature space between two states, $F_r$, amplitude, $\alpha$, and length-scale, $\rho$. For computational efficiency, I utilize a [non-centered parameterization](https://mc-stan.org/docs/stan-users-guide/reparameterization.html) via a [cholesky decomposition](https://mc-stan.org/docs/functions-reference/matrix_operations.html#cholesky-decomposition) of the covariance matrix, $L_r$, and random deviates from the mean, $\eta_r$.

$$
\begin{align*}
\beta_r &= L_r \eta_r \\
K_r &= L_r L_r^{\top} \\
K_r &= \text{Exponentiated Quadratic}(F_r, \alpha, \rho)
\end{align*}
$$

The time-varying parameter, $\beta_{r,d}$, is the result of a correlated random walk, allowing similar states to drift in similar ways over time. The correlation matrix of the random walk is simply scaled down by a factor of $\phi$ (or $\sqrt{\phi}$ in the non-centered parameterization) so that the estimated level of support does not change drastically day-over-day.

$$
\begin{align*}
\beta_{r,d} &= \sum_1^d \sqrt{\phi} L_r \eta_{r,d}
\end{align*}
$$

Polling bias, $\delta_{i,s}$ is comprised of several sources:

* $\beta_b$: state-level polling bias
* $\beta_g$: polling group/polling population bias
* $\beta_m$: poll mode bias
* $\beta_c$: candidate-support (i.e., if the poll was sponsored by a particular party)
* $\beta_p$: pollster bias
* $\beta_n$: a parameter to capture other unmodeled polling biases

Thus, for each poll, $i$, $\delta_{i,s}$ is:

$$
\begin{align*}
\delta_{i,s} &= \beta_{b[i]} + \beta_{g[i]} + \beta_{m[i]} + \beta_{c[i]} + \beta_{p[i]} + \beta_{n[i]}
\end{align*}
$$

Much like previous state parameters, the state bias parameter is the dot product of a vector of "raw" parameters and a vector of weights:

$$
\begin{align*}
\beta_{b[s]} &= \beta_{br} \cdot w_s
\end{align*}
$$

The raw state bias parameters are also modeled as correlated and utilize the same correlation matrix as before, scaled by a factor of $\psi$ (or $\sqrt{\psi}$ in the non-centered parameterization). 

$$
\begin{align*}
\beta_{br} &= \sqrt{\psi} L_r \eta_{br}
\end{align*}
$$

$\beta_m$, $\beta_p$, and $\beta_n$ are modeled as draws from hierarchical distributions with non-centered parameterizations:

$$
\begin{align*}
\beta_m &= \eta_m \sigma_m \\
\beta_p &= \eta_p \sigma_p \\
\beta_n &= \eta_n \sigma_n
\end{align*}
$$

The remaining parameters, $\beta_g$ and $\beta_c$, have too few subgroups to be modeled hierarchically. As such, they are modeled as fixed effects with the "likely voter" group as the reference condition in $\beta_g$ and "no sponsor" as the reference condition in $\beta_c$. 

The polling model generates a series of outputs that are referenced throughout the site:

* Biden's forecasted election-day two-party voteshare in each state: $\theta_{s,D} = \alpha_s + \beta_s + \beta_{s,D}$
* Whether (or not) Biden wins in each state: $W_s = 1 \text{if } \theta_{s,D} > 0.5$, otherwise $W_s = 0$
* The number of electors Biden wins: $E = W \cdot e$, where $e$ is a vector of electors for each state
* Whether (or not) Biden wins the electoral college: $P = 1 \text{if } E > 269$, otherwise $P = 0$
* Whether (or not) there is a tie in the electoral college: $T = 1 \text{if } E = 269$, otherwise $T = 0$

When summarized across draws, these give the median, 66, and 95% credible intervals for Biden's forecasted voteshare in each state and his forcasted number of electors won, and probabilities for a win in each state, a win in the electoral college, and a tie in the electoral college. 

## functions.stan

Contains a set of functions used to support the poll model. Function documentation is included in the script.

