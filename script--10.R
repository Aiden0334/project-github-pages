# =============================================================
# Metadata
# Name: Youngjae Cho
# Date: 2025-04-24
# Purpose: STAT4860 - Assignment 10 for regression and ethanol analysis.
# =============================================================

# --------------------------
# Load data set
getwd()
setwd("C:/Users/choyo/Downloads")
data <- read.csv("gas_mileage_data.csv")
# --------------------------

# --------------------------
# Question 1
# --------------------------

# 1.1 The # of observations after filtering.
data_filtered <- subset(data, !is.na(mpg) & !is.na(ethanol))
data_filtered$ethanol <- ifelse(data_filtered$ethanol == 0, "No", "Yes")
data_filtered$ethanol <- factor(data_filtered$ethanol, levels = c("No", "Yes"))
q1.1 <- nrow(data_filtered)

# 1.2 Proportion of observations with ethanol
q1.2 <- mean(data_filtered$ethanol == "Yes")

# 1.3 The mean mpg when gas has ethanol.
q1.3 <- mean(data_filtered$mpg[data_filtered$ethanol == "Yes"])

# 1.4 The mean mpg when gas does not have ethanol.
q1.4 <- mean(data_filtered$mpg[data_filtered$ethanol == "No"])

# 1.5 The intercept and coefficient for ethanol.
q1.5 <- lm(mpg ~ ethanol, data = data_filtered)$coefficients

# 1.6 The 95% CI for difference in mpg.
q1.6 <- confint(lm(mpg ~ ethanol, data = data_filtered))["ethanolYes", ]

# 1.7 The intercept and coefficient for log(mpg) ~ ethanol.
q1.7 <- lm(log(mpg) ~ ethanol, data = data_filtered)$coefficients

# 1.8 The 95% CI for multiplicative effect.
q1.8 <- exp(confint(lm(log(mpg) ~ ethanol, data = data_filtered))["ethanolYes", ])

# Print all
q1.1; q1.2; q1.3; q1.4; q1.5; q1.6; q1.7; q1.8