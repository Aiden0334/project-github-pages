# =============================================================
# Metadata
# Name: Youngjae Cho
# Date: 2025-04-14
# Purpose: STAT4860 glms Assignment #9
# =============================================================

# --------------------------
# Importing dataset
ex2011 <- Sleuth3::ex2011
ex2225 <- Sleuth3::ex2225
ex2115 <- Sleuth3::ex2115
# --------------------------

# --------------------------
# Question 1
# --------------------------

# 1.1 Identify the response variable
q1.1 <- "Failure"

# 1.2 Identify the explanatory variable
q1.2 <- "Temperature"

# 1.3 Random sample?
q1.3 <- FALSE

# 1.4 Randomized experiment?
q1.4 <- FALSE

# Info for data set ex2011 for building logistic regression.
ex2011$Failure_Binary_Q1.5 <- ifelse(ex2011$Failure == "No", 0, 1)
q1.5_glm <- glm(Failure_Binary_Q1.5 ~ Temperature, data = ex2011, family = "binomial")

# 1.5 Intercept
q1.5 <- q1.5_glm$coefficients[1]

# 1.6 Coefficient for Temperature
q1.6 <- q1.5_glm$coefficients[2]

# 1.7 95% CI for odds ratio
q1.7 = exp(confint(q1.5_glm)["Temperature", ])

# Info for data set ex2011 to do logistic regression.
ex2011$Failure_Binary_Q1.8 <- ifelse(ex2011$Failure == "No", 1, 0)
q1.8_glm <- glm(Failure_Binary_Q1.8 ~ Temperature, data = ex2011, family = "binomial")

# 1.8 Intercept
q1.8 <- q1.8_glm$coefficients[1]

# 1.9 Coefficient for Temperature
q1.9 <- q1.8_glm$coefficients[2]

# 1.10 Compare models
q1.10 <- "C"

# --------------------------
# Question 2
# --------------------------

# 2.1 Response variable
q2.1 <- "Mates"

# 2.2 Explanatory variable
q2.2 <- "BodySize"

# 2.3 Random sample?
q2.3 <- FALSE

# 2.4 Randomized experiment?
q2.4 <- FALSE

# Do glm function1. 
q2_glm <- glm(Mates ~ BodySize, data = ex2225, family = "poisson")

# 2.5 Intercept
q2.5 <- q2_glm$coefficients[1]

# 2.6 Coefficient for Size
q2.6 <- q2_glm$coefficients[2]

# 2.7 95% CI for multiplicative effect
q2.7 = exp(confint(q2_glm)["BodySize", ])

# --------------------------
# Question 3
# --------------------------

# 3.1 Variable for "In Favor"
q3.1 <- "InFavor"

# 3.2 Variable for "Not In Favor"
q3.2 <- "NotInFavor"

# 3.3 Random sample?
q3.3 <- TRUE

# 3.4 Randomized experiment?
q3.4 <- FALSE

# Do glm function2. 
q3_glm <- glm(cbind(InFavor, NotInFavor) ~ Context + Mode + Level, data = ex2115, family = binomial)

# 3.5 Wald test p-value for LevelHigh
q3.5 <- summary(q3_glm)$coefficients["Levellow", "Pr(>|z|)"]

# 3.6 Drop-in-deviance test p-value for Level
q3.6 <- anova(q3_glm, test = "Chisq")[grep("^Level$", rownames(anova(q3_glm, test = "Chisq"))), "Pr(>Chi)"]

# 3.7 95% CI for odds ratio Context Vietnam over Cuba
q3.7 <- exp(confint(q3_glm)["ContextVietnam", ])

# 3.8 95% CI for odds ratio Mode scattered over not
q3.8 <- exp(confint(q3_glm)["Modescattered", ])

# Compare two plots: one is below, second one is in pdf file. eventually I am sure to say that 
# they are same plots. 
newdata <- expand.grid(Context = levels(ex2115$Context), Mode = levels(ex2115$Mode), Level = levels(ex2115$Level))

pred <- predict(q3_glm, newdata = newdata, type = "link", se.fit = TRUE)

newdata$fit <- pred$fit
newdata$se.fit <- pred$se.fit
newdata$InFavor <- plogis(newdata$fit)
newdata$lwr <- plogis(newdata$fit - 1.96 * newdata$se.fit)
newdata$upr <- plogis(newdata$fit + 1.96 * newdata$se.fit)

# Same plot with the picture in pdf file. 
library(ggplot2)

q3.9 <- ggplot(newdata, aes(x = Context, y = InFavor, color = Mode, shape = Level)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), 
                  position = position_dodge(width = 0.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Does the context affect the answer?", 
       y = "InFavor") +
  theme_minimal()



