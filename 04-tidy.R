# =============================================================
# Metadata
# Title: "04-tidy.R"
# Name: "Youngjae Cho"
# Date: "2025-02-27"
# Purpose: Create data frame using the diamonds data set.
# =============================================================

# --------------------------
# Library setting
# --------------------------
library(tidyverse)

# --------------------------
# Question 1-1: Rename price to 'Price' and leave the diamonds data set untouched.
# --------------------------
q1.1 <- diamonds |> rename(Price = price)
q1.1

# --------------------------
# Question 1-2: Rename the columns, for price, carat, cut, color, and clarity.
# --------------------------
q1.2 <- diamonds |> rename(Price = price, Carat = carat, Cut = cut, Color = color, Clarity = clarity)
q1.2

# --------------------------
# Question 1-3: Create 'Dollar per carat' that is divided by carat.
# --------------------------
q1.3 <- diamonds |> mutate("Dollar per carat" = price / carat)
q1.3

# --------------------------
# Question 1-4: Filter Ideal cut.
# --------------------------
q1.4 <- diamonds |> filter(cut == "Ideal")
q1.4

# --------------------------
# Question 1-5: Create data frame and select x, y, z columns.
# --------------------------
q1.5 <- diamonds |> select(x, y, z)
q1.5

# --------------------------
# Question 1-6: Build an exclude depth and table
# --------------------------
q1.6 <- diamonds |> select(-depth, -table)
q1.6

# --------------------------
# Question 1-7: Filter diamonds with 4 qualities: carat>1.5, 
# Good, Premium, or Ideal quality, whose color is not D, and
# whose price is strictly between $10,000 to $15,000. 
# --------------------------
q1.7 <- diamonds |>
  filter(carat > 1.5,
         cut %in% c("Good", "Premium", "Ideal"),
         !color %in% c("D"),
         price > 10000 & price < 15000)
q1.7

# --------------------------
# Question 1-8: Select price, carat, x_cm for Premium quality
# Only diamonds with Premium quality, only diamonds whose width >= 5. 
# --------------------------
q1.8 <- diamonds |>
  mutate(x_cm = x/10) |>
  filter(cut == "Premium", y >= 5) |>
  select(price, carat, x_cm)
q1.8

# --------------------------
# Question 1-9: Calculate summary statistics for price.
# --------------------------
q1.9 <- diamonds |>
  summarise(n=n(), mean = mean(price), sd = sd(price))
q1.9

# --------------------------
# Question 1-10: Filter the 'J' color and 'IF' clarity and summarize carat.
# --------------------------
q1.10 <- diamonds |> 
  filter(color == "J", clarity == "IF") |>
  summarise(n=n(), mean = mean(carat), sd = sd(carat))
q1.10

# --------------------------
# Question 1-11: Group_by cut, color, and clarity, and finally summarize for price.
# --------------------------
q1.11 <- diamonds |>
  group_by(cut, color, clarity) |>
  summarise(n=n(), mean = mean(price), sd=sd(price), .groups = "drop")
q1.11

# --------------------------
# Question 1-12: Use following data frame in 04-tidy.pdf file with original data frame
# so that you have the additional explanation column. (Join)
# --------------------------
clarity_explanation <- tribble(
  ~clarity, ~explanation,
  "I1", "Included",
  "SI1", "Slightly Included 1",
  "SI2", "Slightly Included 2",
  "VS1", "Very Slightly Included 1",
  "VS2", "Very Slightly Included 2",
  "VVS1", "Very, Very Slightly Included 1",
  "VVS2", "Very, Very Slightly Included 2",
  "IF", "Internally Flawless"
)

q1.12 <- diamonds |> left_join(clarity_explanation, by = "clarity")
q1.12

# --------------------------
# Question 1-13: Use the following data frame to filter the observations 
# we have the color and clarity combinations in the table below. 
# --------------------------
combos <- tribble(
  ~color, ~clarity,
  "E", "SI1",
  "E", "VS2",
  "F", "VVS2",
  "F", "IF",
  "H", "I1"
)

q1.13 <- diamonds |> semi_join(combos, by = c("color", "clarity"))
q1.13

# --------------------------
# Question 1-14: Use the combos data frame to remove those color and clarity combinations.
# --------------------------
q1.14 <- diamonds |> anti_join(combos, by = c("color", "clarity"))
q1.14

# --------------------------
# Question 1-15: Summarize combos for price for each color-clarity combinations.
# --------------------------
q1.15 <- diamonds |>
  semi_join(combos, by = c("color", "clarity")) |>
  group_by(color, clarity) |>
  summarise(n=n(), mean = mean(price), sd = sd(price), .groups = "drop")
q1.15
