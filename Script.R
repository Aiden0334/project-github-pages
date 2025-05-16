# =============================================================
# Metadata
# Name: Youngjae Cho
# Date: 2025-04-24
# Purpose: STAT4860 - Homework for regression and ethanol analysis.
# =============================================================

# --------------------------
# Data set name: gas_mileage_data.
data <- read.csv("C:/Users/choyo/OneDrive - Iowa State University/gas_mileage_data.csv")
# --------------------------

# --------------------------
# Filter the data set
filtered_data <- subset(data, !is.na(mpg) & !is.na(ethanol))
filtered_data$ethanol <- ifelse(filtered_data$ethanol == 0, "No", "Yes")
filtered_data$ethanol <- factor(filtered_data$ethanol, levels = c("No", "Yes"))
# --------------------------

# 1.1 The # of observations after filtering.
q1.1 <- nrow(filtered_data)

# 1.2 Proportion of observations with ethanol.
q1.2 <- mean(filtered_data$ethanol == "Yes")

# 1.3 Mean mpg when gas has ethanol.
q1.3 <- mean(filtered_data$mpg[filtered_data$ethanol == "Yes"])

# 1.4 Mean mpg when gas does not have ethanol.
q1.4 <- mean(filtered_data$mpg[filtered_data$ethanol == "No"])

# 1.5 Regression: mpg ~ ethanol.
q1.5 <- lm(mpg ~ ethanol, data = filtered_data)$coefficients

# 1.6 The 95% Confidence Interval for difference in mpg.
q1.6 <- confint(lm(mpg ~ ethanol, data = filtered_data))["ethanolYes", ]

# 1.7 Regression: log(mpg) ~ ethanol.
q1.7 <- lm(log(mpg) ~ ethanol, data = filtered_data)$coefficients

# 1.8 The 95% CI for multiplicative effect for ethanol.
q1.8 <- exp(confint(lm(log(mpg) ~ ethanol, data = filtered_data))["ethanolYes", ])