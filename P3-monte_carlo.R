#-------------------------------------------------------------------------------
# Metadata
# Title: "Portfolio 3: Monte Carlo Simulation Study"
# Author:  Youngjae Cho
# Date:    2025-05-03 ~ 2025-05-09
# Purpose: R Markdown file for Portfolio 3: Monte Carlo Study.
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")
library(tidyverse)

# For parameters.
set.seed(12345)
n_sim <- 1000 
n <- 20 
p_values <- seq(0.1, 0.9, by =0.1)


sim_results <- expand.grid(p = p_values, sim = 1:n_sim) %>%
  rowwise() %>%
  mutate(
    x = rbinom(1, n, p),
    freq_est = x / n,
    bayes_est = (x + 1) / (n + 2),
    freq_lower = prop.test(x, n)$conf.int[1],
    freq_upper = prop.test(x, n)$conf.int[2],
    bayes_lower = qbeta(0.025, x + 1, n - x + 1),
    bayes_upper = qbeta(0.975, x + 1, n - x + 1),
    freq_cover = p >= freq_lower & p <= freq_upper,
    bayes_cover = p >= bayes_lower & p <= bayes_upper
  ) %>%
  ungroup()

# Build a MSE Summary for frequentist and Bayesian methods. 
mse_summary <- sim_results %>%
  group_by(p) %>%
  summarize(
    mse_freq = mean((freq_est - p)^2),
    mse_bayes = mean((bayes_est - p)^2),
    .groups = "drop"
  ) %>%
  pivot_longer(-p, 
               names_to = "method", 
               values_to = "mse")

# Build ggplot for using mse_summary for frequentist and Bayesian. 
ggplot(mse_summary, aes(x = p, y = mse, color = method)) +
  geom_line(size = 1) +
  labs(
    title = "Mean Squared Error between frequentist and Bayesian method",
    x = "True Probability",
    y = "MSE",
    color = "Method"
  ) + theme_minimal()

coverage_summary <- sim_results %>%
  group_by(p) %>%
  summarize(
    #Average mean coverage for frequentist method.
    cover_freq = mean(freq_cover),   
    #Average mean coverage for Bayesian method.
    cover_bayes = mean(bayes_cover), 
    .groups = "drop"
  ) %>%
  pivot_longer(-p, names_to = "method", values_to = "coverage")


ggplot(coverage_summary, 
       aes(x = p, 
           y = coverage,
           color = method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(
    title = "Coverage Probability between True Probability and Coverage.",
    x = "True Probability",
    y = "Coverage",
    color = "Method"
  ) + theme_minimal()

sim_results <- sim_results %>%
  mutate(
    sq_err_freq = (freq_est - p)^2,
    sq_err_bayes = (bayes_est - p)^2
  )

mse_summary <- sim_results %>%
  group_by(p) %>%
  summarize(
    mse_freq = mean(sq_err_freq),
    mse_bayes = mean(sq_err_bayes),
    se_freq = sd(sq_err_freq) / sqrt(n_sim),
    se_bayes = sd(sq_err_bayes) / sqrt(n_sim),
    .groups = "drop"
  )


coverage_summary <- sim_results %>%
  group_by(p) %>%
  summarize(
    cover_freq = mean(freq_cover),
    cover_bayes = mean(bayes_cover),
    se_cover_freq = sd(freq_cover) / sqrt(n_sim),
    se_cover_bayes = sd(bayes_cover) / sqrt(n_sim),
    .groups = "drop"
  )

mse_summary <- mse_summary %>%
  mutate(
    lower_freq = mse_freq - 1.96 * se_freq,
    upper_freq = mse_freq + 1.96 * se_freq,
    lower_bayes = mse_bayes - 1.96 * se_bayes,
    upper_bayes = mse_bayes + 1.96 * se_bayes
  )


coverage_summary <- coverage_summary %>%
  mutate(
    lower_cover_freq = cover_freq - 1.96 * se_cover_freq,
    upper_cover_freq = cover_freq + 1.96 * se_cover_freq,
    lower_cover_bayes = cover_bayes - 1.96 * se_cover_bayes,
    upper_cover_bayes = cover_bayes + 1.96 * se_cover_bayes
  )
