# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(ggdist)
library(riekelib)

# support functions
source("R/imports.R")

# binary color palette
# pal <- c("#D78080", "#7CB0D7")
pal <- c("#B13737", "#3579AC")

# import data ------------------------------------------------------------------

abramovitz <- read_csv("data/abramovitz.csv")
cpi <- fetch_cpi()
gdp <- fetch_gdp()

# wrangle economic data --------------------------------------------------------

max_cpi <-
  cpi %>%
  filter(DATE == max(DATE)) %>%
  pull(CPIAUCSL)

cpi <-
  cpi %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(inflation = max_cpi/cpi)

# adjust gdp for inflation
real_gdp <-
  gdp %>%
  rename_with(str_to_lower) %>%
  left_join(cpi) %>%
  select(-cpi) %>%
  mutate(real_gdp = gdp*inflation) %>%
  select(date, real_gdp)

# append national data with 2nd quarter real annualized gdp growth
abramovitz <-
  abramovitz %>%
  mutate(begin_quarter = mdy(paste0("7/1/", year - 1)),
         end_quarter = mdy(paste0("7/1/", year))) %>%
  left_join(real_gdp, by = c("begin_quarter" = "date")) %>%
  rename(begin_gdp = real_gdp) %>%
  left_join(real_gdp, by = c("end_quarter" = "date")) %>%
  rename(end_gdp = real_gdp) %>%
  mutate(real_gdp_growth = end_gdp/begin_gdp - 1) %>%
  select(-ends_with("quarter"),
         -ends_with("gdp"))

# prep for stan ----------------------------------------------------------------

# summarize abramovitz data
model_data <-
  abramovitz %>%
  mutate(fte_net_inc_approval = fte_net_inc_approval/100) %>%
  select(year,
         inc_party,
         I = inc_running,
         dem,
         rep,
         A = fte_net_inc_approval,
         G = real_gdp_growth) %>%
  transmute(V = if_else(inc_party == "dem", dem/(dem + rep), rep/(dem + rep)),
            A = A,
            G,
            I)

# get most recent real gdp growth
G_new <-
  real_gdp %>%
  filter(date %in% c(max(date),
                     ymd(paste(year(max(date)) - 1,
                               month(max(date)),
                               day(max(date)),
                               sep = "-")))) %>%
  arrange(date) %>%
  mutate(real_gdp_growth = real_gdp/lag(real_gdp) - 1) %>%
  drop_na() %>%
  pull(real_gdp_growth)

# pass to stan
stan_data <-
  list(
    N = nrow(model_data),
    V = model_data$V,
    A = model_data$A,
    G = model_data$G,
    I = model_data$I,
    A_new = fetch_approval(),
    G_new = G_new,
    I_new = 1
  )

# compile & fit
prior_model <-
  cmdstan_model("stan/prior_model.stan")

prior_fit <-
  prior_model$sample(
  data = stan_data,
  seed = 2024,
  iter_warmup = 2000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 4
)

# blegh ------------------------------------------------------------------------

prior_fit$draws(variables = "theta_pred", format = "df") %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta_pred"),
               names_to = "rowid",
               values_to = "theta_pred") %>%
  nest(data = -rowid) %>%
  select(-rowid) %>%
  bind_cols(abramovitz %>% transmute(year = as.character(year),
                                     inc_party),
            .) %>%
  unnest(data) %>%
  mutate(theta_pred = if_else(inc_party == "dem", theta_pred, 1 - theta_pred)) %>%
  ggplot(aes(x = year,
             y = theta_pred)) +
  stat_gradientinterval(aes(fill = after_stat(y > 0.5)),
                        fill_type = "gradient") +
  geom_point(data = model_data %>%
               bind_cols(abramovitz) %>%
               mutate(V = if_else(inc_party == "dem", V, 1 - V)),
             mapping = aes(x = as.character(year),
                           y = V),
             color = "white",
             fill = "gray",
             size = 3,
             shape = 21) +
  scale_fill_manual(values = pal) +
  scale_y_percent() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**A Prior for Presidential Pushes for Power**",
       subtitle = "National two-party vote share and posterior predictive fit",
       x = NULL,
       y = NULL,
       caption = paste("Pointrange indicates 66% and 95% posterior",
                       "credible intervals based on 8,000 MCMC samples",
                       sep = "<br>"))

ggquicksave("fig/prior_model_post_predictive.png")

theta_new_pred <-
  prior_fit$draws("theta_new_pred", format = "df") %>%
  as_tibble()

limits <- c(min(theta_new_pred$theta_new_pred), max(theta_new_pred$theta_new_pred))

theta_new_pred %>%
  ggplot(aes(x = theta_new_pred,
             fill = after_stat(x > 0.5))) +
  stat_histinterval(alpha = 0.75,
                    breaks = seq(from = limits[1],
                                 to = limits[2],
                                 length.out = 70),
                    slab_size = 0.5,
                    outline_bars = TRUE) +
  scale_fill_manual(values = pal) +
  scale_x_percent(breaks = seq(from = 0, to = 1, length.out = 21)) +
  theme_rieke() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "**asd;lkfjasd;lkfj**",
       subtitle = "a;lksdjfa;lskdjf",
       x = NULL,
       y = NULL,
       caption = "asd;lkfjasd;lkfj")

