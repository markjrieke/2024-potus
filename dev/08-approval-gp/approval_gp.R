# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)

approval <-
  read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv")

# bleggh -----------------------------------------------------------------------

approval %>%
  select(date = end_date,
         approve = approve_estimate,
         disapprove = disapprove_estimate) %>%
  mutate(net = approve - disapprove) %>%
  mutate(net = logit(net, -100, 100)) %>%
  ggplot(aes(x = date,
             y = net)) +
  geom_line() +
  theme_rieke()

cov_periodic <- function(x,
                         amplitude = 1,
                         length_scale = 1,
                         period = 1,
                         delta = 1e-09) {

  S <- matrix(nrow = length(x), ncol = length(x))
  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- amplitude * exp(-2*(sin(pi*abs(x[i] - x[j])/period)^2)/length_scale)
    }
    S[i,i] <- S[i,i] + delta
  }
  return(S)
}

cov_linear <- function(x,
                       slope = 0,
                       intercept = 0,
                       constant = 0,
                       delta = 1e-09) {

  S <- matrix(nrow = length(x), ncol = length(x))
  for (i in 1:nrow(S)) {
    for(j in 1:ncol(S)) {
      S[i,j] <- intercept + slope*(x[i] - constant)*(x[j] - constant)
    }
    S[i,i] <- S[i,i] + delta
  }
  return(S)
}

sample_gp <- function(covariance,
                      n = 100) {

  samples <-
    MASS::mvrnorm(
      n,
      rep(0, nrow(covariance)),
      covariance
    )

  t(samples) %>%
    as_tibble() %>%
    rowid_to_column("idx") %>%
    pivot_longer(starts_with("V"),
                 names_to = "sample",
                 values_to = "value") %>%
    ggplot(aes(x = idx,
               y = value,
               group = sample)) +
    geom_line(alpha = 0.125) +
    theme_rieke()

}

x <- seq(from = -1, to = 1, length.out = 100)

S1 <- cov_exp_quad(x, length_scale = 0.1)
S2 <- cov_periodic(x, length_scale = 1, period = 0.5)
S3 <- cov_linear(x, slope = 2, intercept = 0, constant = 0)

sample_gp(S1 + S3)#,
          n = 3) #+
  facet_wrap(~sample,
             ncol = 1)
