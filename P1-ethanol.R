#-------------------------------------------------------------------------------
# Metadata
# Title: "Portfolio 1: Ethanol"
# Author:  Youngjae Cho
# Date:    2025-04-29
# Purpose: R Markdown file for Portfolio 1: Ethanol Analysis
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")

# Create html
# rmarkdown::render("P1-ethanol.Rmd")

# Create Rscript
knitr::purl("P1-ethanol.Rmd", documentation = 0L)

data <- read.csv("gas_mileage_data.csv", stringsAsFactors = FALSE)
data <- data[!is.na(data$mpg) & !is.na(data$ethanol), ]
data$ethanol[data$ethanol > 0 ] <- "YES"
data$ethanol[data$ethanol == 0] <- "NO"
data$ethanol <- factor(data$ethanol)

# Calculate summary statistics - counts, mean mpg, sd_mpg, by combining the mean and sd to build a new object "mean_sd."
s <- data %>%
  group_by(ethanol) %>%
  summarize(
    n = n(),
    mean_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    .groups = "drop"
  ) %>%
  mutate(
    mean_sd = paste0(
      format(round(mean_mpg, 1), nsmall = 1),
      " (",
      format(round(sd_mpg, 1), nsmall = 1),
      ")"
    ))

# Create HTML table from summary statistics
s %>%
  pivot_wider(
    names_from = ethanol,
    values_from = mean_sd
  ) %>%
  knitr::kable(
    caption = "Mean (SD) of Miles Per Gallon for Ethanol vs Non-Ethanol Gasoline",
      # label   = "summary-statistics",
    align = c("rlll")) # Column alignment: [r]ight or [l]eft

# Create exploratory plot of the data
ggplot(data,
       aes(
         x = ethanol,
         y = mpg,
         color = ethanol,
         shape = ethanol
       )) +
  geom_point(
    position = position_jitterdodge(
      dodge.width = 0.1,
      jitter.width = 0.05
    )
  ) +
  labs(
    x = "Presence of Ethanol (Yes or No)",
    y = "Miles Per Gallon (mpg)",
    title = "Gas Mileage by Ethanol Usage"
  )

# I want to use Normal family but in R, there was an error to use this. 
# Therefore, I would use gaussian family with this continuous variable, mpg. 
m <- glm(mpg ~ ethanol, 
         data = data, 
         family = gaussian()
)

data_means <- data %>%
  group_by(ethanol) %>%
  summarize(
    n_group = n(),
    mean_mpg = mean(mpg),
    se_mpg = sd(mpg) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_mpg - qt(0.975, n_group - 1) * se_mpg,
    upper = mean_mpg + qt(0.975, n_group - 1) * se_mpg
  )

ggplot(data_means, aes(x = ethanol, y = mean_mpg)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_line(aes(group = 1)) +
  labs(
    x = "Presence of Ethanol (Yes or No)",
    y = "MPG",
    title = "Estimated Mean MPG with 95% Confidence Intervals",
    subtitle = "Linear Regression Model"
  )

# Construct confidence interval to answer scientific question
ci <- data_means %>%
  filter(ethanol == "YES") %>%
  select(lower, upper) %>%
  mutate(
    lower = round(lower, 1),
    upper = round(upper, 1))
