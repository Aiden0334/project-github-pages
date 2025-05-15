#-------------------------------------------------------------------------------
# Metadata
# Title: "Portfolio 2: Wine Quality Analysis"
# Author:  Youngjae Cho
# Date:    2025-05-03
# Purpose: R Markdown file for Portfolio 2: Wine Quality Analysis
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")

data <- read.csv("winequalityN.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(quality), !is.na(alcohol), !is.na(pH)) %>%
  mutate(
    pH_group = cut(pH, breaks = c(2.7, 3.1, 3.4, 3.8, 4.0),
                   labels = c("low", "medium-low", "medium-high", "high")),
    alcohol_group = cut(alcohol, breaks = c(8, 10, 11, 12, 15),
                        labels = c("low", "medium-low", "medium-high", "high"))
  )

# Calculate summary statistics - alcohol-pH combination.
s <- data %>%
  group_by(alcohol_group, pH_group) %>%
  summarize(
    n = n(),
    mean = mean(quality),
    sd = sd(quality),
    .groups = "drop"
  ) %>%
  mutate(
    mean_sd = paste0(
      format(round(mean,1), nsmall =1), " (", format(round(sd,1), nsmall =1), ")"
    )
  )

# Create HTML table from summary statistics
s %>%
  pivot_wider(
    names_from = pH_group,
    values_from = mean_sd
  ) %>%
  knitr::kable(
    caption = "Average Wine Quality between Alcohol and pH Group",
    # label   = "summary - statistics",
    align = c("rlllll")
  )

# Create exploratory plot of the data
ggplot(data, aes(x = alcohol_group, y = quality, color = pH_group)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.2, 
                                             jitter.width = 0.1), alpha = 0.6) +
  labs(
    x = "Alcohol Group",
    y = "Quality Score",
    title = "Wine Quality Score by Alcohol Group and pH Group",
    color = "pH Group"
  )

# I want to use Normal family but in R, there was an error to use this. 
# Therefore, I would use gaussian family with this continuous variable, wine quality. 
m <- glm(quality ~ alcohol + pH, 
         data = data, 
         family = gaussian())

data_means <- data %>%
  group_by(alcohol_group, pH_group) %>%
  summarize(
    n_group = n(),
    mean_quality = mean(quality),
    se_quality = sd(quality) / sqrt(n_group),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_quality - qt(0.975, n_group - 1) * se_quality,
    upper = mean_quality + qt(0.975, n_group - 1) * se_quality
  )

ggplot(data_means, aes(x = alcohol_group, 
                       y = mean_quality, 
                       color = pH_group, 
                       group = pH_group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(0.4)) +
  geom_line(position = position_dodge(0.4)) +
  labs(
    x = "Alcohol Group",
    y = "Average Mean of Wine Quality Score",
    title = "Estimated Mean Wine Quality with 95% Confidence Intervals",
    color = "pH Group"
  ) +
  theme_minimal()

# Construct confidence interval to answer scientific question
ci <- data_means %>%
  filter(alcohol_group == "high", pH_group == "medium-high") %>%
  select(lower, upper) %>%
  mutate(
    lower = round(lower, 2),
    upper = round(upper, 2))
