---
  # title: "STAT 4860 - Homework Assignment #3"
  # author: "Youngjae Cho"
  # date: "2025-02-13"
  # purpose: Submit visualizations for question 1, 2, and 3
---
  #Example for submitting visualizations
  # Code to create my plot
  # g <- ggplot(data = data.frame(normals=rnorm(10))) + geom_histogram(mapping = aes(x = normals))
  # g
  
  # Code to save my plot
  # ggsave(plot=g, 
  #       filename = "filename.jpeg",
  #       width = 6,
  #       height = 4)

#1---------------------------------------------------------------
#Using data set, 'longevity.csv'that contains lifetime (in months) depending on diet.
#diets include "NP", "N/N85", "N/N50", "N/N50 lopro", "R/R50", "N/R40."
#Construct six violin plots, one for each Diet, for the Lifetime.
#Plot Criteria: x-axis = diet in the order listed above, 
#y-axis is Lifetime, y-axis label includes units, a relevant title is included in "title case",
#6 violin plots are shown (no faceting), "minimal" theme is used.
library(ggplot2)

# Change file path to my own storage.
setwd("C:/Users/choyo/OneDrive - Iowa State University/")

# Read the 'longevity' data set.
longevity <- read.csv("longevity.csv")
longevity

longevity$Diet <- factor(longevity$Diet, levels = c("NP", "N/N85", "N/N50", "N/N50 lopro", "R/R50", "N/R40"))
longevity$Diet

# Build six violin Plots, one for each Diet, for the Lifetime.
# Diet Type - "NP"
g1 <- ggplot(longevity, aes(x = "NP", y = Lifetime)) +
  geom_violin(fill = "blue", color = "red") +
  labs(
       title = "Violin Plot from Lifetime on Diet Type, NP",
       x = "NP",
       y = "Lifetime (Months)") +
  theme_minimal()
g1


ggsave(plot=g1, 
       filename = "Question 1 - First Violin Plot, using Diet Type, NP.jpeg",
       width = 6,
       height = 4)

# Diet Type -  "N/N85"
g2 <- ggplot(longevity, aes(x = "N/N85", y = Lifetime)) +
  geom_violin(fill = "blue", color = "red") +
  labs(
       title = "Violin Plot from Lifetime on Diet Type, N/N85",
       x = "N/N85",
       y = "Lifetime (Months)") + theme_minimal()
g2

ggsave(plot=g2, 
       filename = "Question 1 - Second Violin Plot, using Diet Type, N/N85.jpeg",
       width = 6,
       height = 4)


# Diet Type - "N/N50"
g3 <- ggplot(longevity, aes(x = "N/N50", y = Lifetime)) +
  geom_violin(fill = "blue", color = "red") +
  labs(
    title = "Violin Plot from Lifetime on Diet Type, N/N50",
    x = "N/N50",
    y = "Lifetime (Months)") + theme_minimal()
g3


ggsave(plot=g3, 
       filename = "Question 1 - Third Violin Plot, using Diet Type, N/N50.jpeg",
       width = 6,
       height = 4)


# Diet Type - "N/N50 lopro"
g4 <- ggplot(longevity, aes(x = "N/N50 lopro", y = Lifetime)) +
  geom_violin(fill = "blue", color = "red") +
  labs(
    title = "Violin Plot from Lifetime on Diet Type, N/N50 lapro",
    x = "N/N50 lapro",
    y = "Lifetime (Months)") + theme_minimal()
g4


ggsave(plot=g4, 
       filename = "Question 1 - Forth Violin Plot, using Diet Type, N/N50 lopro.jpeg",
       width = 6,
       height = 4)


# Diet Type - "R/R50"
g5 <- ggplot(longevity, aes(x = "R/R50", y = Lifetime)) +
  geom_violin(fill = "black", color = "orange") +
  labs(
    title = "Violin Plot from Lifetime on Diet Type, R/R50",
    x = "R/R50",
    y = "Lifetime (Months)") + theme_minimal()
g5


ggsave(plot=g5, 
       filename = "Question 1 - Fifth Violin Plot, using Diet Type, R/R50.jpeg",
       width = 6,
       height = 4)


# Diet Type - "N/R40"
g6 <- ggplot(longevity, aes(x = "N/R40", y = Lifetime)) +
  geom_violin(fill = "black", color = "orange") +
  labs(
    title = "Violin Plot from Lifetime on Diet Type, N/R40",
    x = "N/R40",
    y = "Lifetime (Months)") + theme_minimal()
g6


ggsave(plot=g6, 
       filename = "Question 1 - Sixth Violin Plot, using Diet Type, N/R40.jpeg",
       width = 6,
       height = 4)


#2---------------------------------------------------------------
#Construct Scatterplot using the case0801 data set within the Sleuth3 package.
#Plot Criteria: x-axis has the explanatory varaible and y-axis has response, 
#x-axis and y-axis is logarithmic, x-axis has an appropriate label with units,
#y-axis has an appropriate label, using title, subtitle indicates the location,
#Regression line is included(use stat_smooth(method = "lm))
#No theme_minimal function used.

install.packages("Sleuth3")
library(Sleuth3)
data(case0801)
head(case0801)

g7 <- ggplot(case0801, aes(x= Area, y= Species)) +
  geom_point(color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Area and Species Comparison",
    subtitle = "Data from case0801 in Sleuth3",
    x = "Island Area",
    y = "Number of Species") +
  geom_smooth(method = "lm", color = "orange")
g7


ggsave(plot=g7, 
       filename = "Question 2 - Scatterplot, using case0801 data set within the Sleuth3 package.jpeg",
       width = 6,
       height = 4)


#3---------------------------------------------------------------
#Need to find an appropriate data set, graphic has a following instruction: 
#a continuous variable is represented on y-axis,
#a variable is represented on the x-axis,
#a variable is represented by one of the following: color, shape, linetype,
#x-axis and y-axis label are informative with units,
#legend is included for color, shape, and/or linetype,
#the legend has informative title,
#the graphic has an informative title. 

#I will use one data set from Sleuth3 package: 
#case0702, regarding Meat Processing and PH
library(Sleuth3)
data(case0702)
head(case0702)

g8 <- ggplot(case0702, aes(x = Time, y = pH)) +
  geom_point(color = "blue", size = 1) +
  geom_line(size = 2, color ="red") +
  labs(
       title = "Ph levels for Meat Processing over time",
       x = "Time (hours)",
       y = "pH level"
       ) + theme_minimal()
g8

ggsave(plot = g8,
       filename = "Question3 - Graphics between Time (hours) and pH level over time.jpeg",
       width = 6,
       height = 4)

### Thank you! 




