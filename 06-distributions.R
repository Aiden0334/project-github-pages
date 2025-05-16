# =============================================================
# Metadata
# Title: "06-Distributions.R"
# Name: "Youngjae Cho"
# Date: "2025-03-24"
# Purpose: Calculate expectation, variance, probability with using R code.
# =============================================================

# --------------------------
# Question 1: Y follows Binomial distribution with parameters n = 42, p = 1/3
# --------------------------
n <- 42
p <- 1/3

# --------------------------
# Question 1-1: Calculate E[Y].
# --------------------------
expectation1 <- n*p


# --------------------------
# Question 1-2: Calculate Var[Y].
# --------------------------
variance1 <- n*p*(1-p)


# --------------------------
# Question 1-3: Find P(Y=10).
# --------------------------
prob_y10 <- dbinom(10, size = n, prob = p)


# --------------------------
# Question 1-4: Find P(Y<=12).
# --------------------------
prob_y12 <- pbinom(12, size = n, prob = p)


# --------------------------
# Question 1-5: Find P(2<Y<=20).
# --------------------------
prob_2y20 <- pbinom(20, size = n, prob = p) - pbinom(2, size = n, prob = p)


# --------------------------
# Question 1-6: Find P(Y>=25).
# --------------------------
prob_y25 <- 1 - pbinom(24, size = n, prob = p)



# --------------------------
# Question 2: Two 6-sided dice are rolled and the sum of the two dice determine
# (Binomial dist problems)
# Question 2-1: Sum of two dice = 6, receive wheat, at least 2 wheat in 8 turns. 
# --------------------------
pwheat <- 5/36
prob_8_turns <- 1 - pbinom(1, 8, prob = pwheat)


# --------------------------
# Question 2-2: Sum of two dice = 8, receive wood, P(3 or fewer resources). 
# --------------------------
pres <- (5 + 5 + 2) / 36
prob_pres <- pbinom(3, 8, prob = pres)


# --------------------------
# Question 2-3: 71 turns, rolls a 7, the robber is moved. Expectation needed.
# --------------------------
expectation2 <- 71 * (1/6)


# --------------------------
# Question 3: Let X follows Poisson Distributions with parameter lambda 4.5. 
# Question 3-1: Calculate the P(X=5).
# --------------------------
prob_pos <- dpois(5, lambda = lambda)


# --------------------------
# Question 3-2: Calculate P(4<=X<=10).
# --------------------------
prob_pos2 <- ppois(10, lambda = lambda) - ppois(3, lambda = lambda)


# --------------------------
# Question 3-3: Calculate the P(X even and X < 21).
# --------------------------
even_vals <- seq(0, 20, by = 2)

prob_pos3 <- sum(dpois(even_vals, lambda = lambda))



# --------------------------
# Question 4: # of Downy Woodpeckers in one minute in Poisson Dist (lambda=1).
# Question 4-1: Find probability in 0 woodpeckers in 1 minute.
# --------------------------
prob_pos_woodp1 <- dpois(0, lambda = 1)

# --------------------------
# Question 4-2: Probability of 0 woodpeckers in 5 mins (lambda = 5).
# --------------------------
prob_pos_woodp5 <- dpois(0, lambda = 5)


# --------------------------
# Question 5: Random Variable Y follows Normal Dist with mean 4, variance 36.
# Question 5-1: P(Y=4).
# --------------------------
mu5 <- 4
sigma5 <- sqrt(36)

prob_y4 <- 0


# --------------------------
# Question 5-2: Calculate the probability density function value for when Y = 4.
# --------------------------
prob_d_f_v <- dnorm(4, mean = mu5, sd = sigma5)


# --------------------------
# Question 5-3: Find P(Y < 6).
# --------------------------
prob_y6 <- pnorm(6, mean = mu5, sd = sigma5)


# --------------------------
# Question 5-4: Find P(|Y-4|>2) = P(Y < 2 or Y > 6).
# --------------------------
prob_absolvalue <- pnorm(2, mean = mu5, sd = sigma5) + (1 - pnorm(6, mean = mu5, sd = sigma5))

# --------------------------
# Question 5-5: Find the z-critical value for constructing 99% confidence interval.
# --------------------------
z_critical <- qnorm(0.995)

# --------------------------
# Question 6: Automatic tire pump - standard deviation = 0.3 (psi). 
# Question 6-1: Find the prob(tire pump fills your tire within 0.5 pressure).
# --------------------------
sigma6 <- 0.3
prob_pumpfills <- pnorm(0.5, mean = 0, sd = sigma6) - pnorm(-0.5, mean = 0, sd = sigma6)

# --------------------------
# Question 6-2: Desired pressure = 33 psi, find P(tire pump fills < 31 psi).
# --------------------------
prob_pumpfills2 <- pnorm(31, mean = 33, sd = sigma6)





















