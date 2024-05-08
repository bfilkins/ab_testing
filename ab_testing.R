
library(purrr)
library(tidyr)
library(highcharter)
library(ggplot2)
library(dplyr)

# create fake data sets for x visitors (3K?) over x days (30? fake campaign duration) by effect size difference ####

starting_engagement_rate <- .7
days <- 60
daily_visitors <- 100
test_simulation_count <- 10
ppt_increment_values <- c(.01,.025,.05,.1,.15,.2)

ppt_increments <- function(){tibble(ppt_increment = ppt_increment_values) |>
  mutate(test_day = list(seq(1,30,1))) |>
  unnest_longer(test_day) |>
  rowwise() |>
  mutate(
    a_variant = rbinom(1, daily_visitors, starting_engagement_rate),
    b_variant = rbinom(1, daily_visitors, starting_engagement_rate+ppt_increment),
    a_consolidation = rbinom(1, daily_visitors*2, starting_engagement_rate),
    b_consolidation = rbinom(1, daily_visitors*2, starting_engagement_rate+ppt_increment),
    n = daily_visitors) |>
    ungroup()
  }

iterations <- tibble(iteration = seq(1:test_simulation_count)) |>
  rowwise() |>
  mutate(data = list(ppt_increments())) |>
  ungroup() |>
  unnest(data) |>
  group_by(iteration, ppt_increment) |>
  mutate(
    a_cumulative = cumsum(a_variant),
    b_cumulative = cumsum(b_variant),
    n_cumulative = cumsum(n)
  ) |>
  ungroup()

# Simulate tests and decision-points for:
# -Bayesian approach: odds ratio using monte carlo sampling from Beta distribution

beta_simulation_count <- 4000
ratio_threshold <- 20

beta_simulations <- iterations |>
  #head(200) |>
  rowwise() |>
  mutate(
    a_beta_simulations = map(iteration, ~ rbeta(beta_simulation_count,a_cumulative+1, n_cumulative- a_cumulative)),
    b_beta_simulations = map(iteration, ~ rbeta(beta_simulation_count,b_cumulative+1, n_cumulative- b_cumulative))
  ) |>
  ungroup() |>
  unnest(c(a_beta_simulations, b_beta_simulations)) |>
  mutate(b_greaterer_than_a = if_else(a_beta_simulations < b_beta_simulations,1,0)) |>
  ungroup()

beta_simulations |> 
  select(ppt_increment, test_day, a_beta_simulations, b_beta_simulations) |>
  filter(test_day == 1 | test_day == 10 |test_day == 20 |test_day == 30) |>
  mutate(test_day = paste("test day:", test_day)) |>
  pivot_longer(cols = c(a_beta_simulations,b_beta_simulations)) |>
  ggplot(aes(x = value, y = as.factor(ppt_increment), fill = name)) +
  scale_fill_manual(values = c(pal_vibranium$light_gray,pal_vibranium$electric_blue)) +
  scale_x_continuous(limits = c(.6,1)) +
  ggridges::geom_density_ridges() +
  facet_wrap( facets = "test_day") +
  theme_void()

simulations <- beta_simulations |>
  group_by(iteration,ppt_increment,test_day,a_variant,b_variant,a_consolidation,b_consolidation,n,a_cumulative,b_cumulative,n_cumulative) |>
  summarise(b_greater = sum(b_greaterer_than_a)) |>
  ungroup() |>
  mutate(
    b_greaterer_likelihood_ratio = (b_greater/beta_simulation_count)/((beta_simulation_count-b_greater)/beta_simulation_count),
    a_greater_likelihood_ratio = ((beta_simulation_count-b_greater)/beta_simulation_count)/(b_greater/beta_simulation_count),
    threshold_x = if_else(b_greaterer_likelihood_ratio > ratio_threshold | a_greater_likelihood_ratio > ratio_threshold, test_day, days+1)
    ) |>
  group_by(iteration,ppt_increment) |>
  mutate(
    bayesian_decision_day = min(threshold_x),
    a_or_b_adoption = if_else(test_day == bayesian_decision_day & b_greaterer_likelihood_ratio > ratio_threshold, "b", "a")
    ) |>
  ungroup() |>
  mutate(bayes_decision_k = case_when(
    test_day <= bayesian_decision_day ~ a_variant + b_variant,
    test_day > bayesian_decision_day & a_or_b_adoption == "a" ~ a_consolidation,
    test_day > bayesian_decision_day & a_or_b_adoption == "b" ~ b_consolidation))
