---
# title: "STAT 4860 - Homework Assignment #2"
# author: "Youngjae Cho"
# date: "2025-02-11"
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

# Plots can be found here
# getwd()

#1----------------------------------------------------------------
#Construct a scatterplot using the 'iris' data set. 
#Instruction: petal length on the x-axis (horizontal), petal width on the y-axis (vertical),
#species as a shape, sepal length as a color, informative x-axis label, informative y-axis label, 
#informative legend titles that have no periods and including units, a title that includes the name of the individual who collected the data,
#a title that includes the year the data were collected, x-axis that ranges from 0 to 8, y-axis that ranges from 0 to 3.
library(ggplot2)
# Build Scatterplot of the iris data set. 

g1 <- ggplot(iris, aes(x=Petal.Length, y= Petal.Width, shape = Species, color = Sepal.Length)) +
      geom_point() + scale_x_continuous(limits=c(0, 8)) +
      scale_y_continuous(limits=c(0, 3)) +
      labs(
        title = "Scatter Plot of Iris data set",
        x = "Petal Length",
        y = "Petal Width",
        color = "Sepal Length",
        shape = "Species"
      ) + theme_minimal()
g1
#Save this scatterplot to my file.
ggsave(plot = g1, 
       filename = "Question 1 - Scatter Plot of iris data set.jpeg",
       width = 6, 
       height = 4)

getwd()



#2----------------------------------------------------------------
#Construct faceted scatterplots using the 'Indometh' data set. 
#Instruction: time on the x-axis(horizontal), concentration on the y-axis(vertical), informative x-axis label including units,
#informative y-axis label including units, informative title, x-axis on a logarithmic scale, y-axis on a logarithmic scale,
#faceted by subject, facet titles include "Subject:" and the subject number. 
library(datasets)

# Build faceted Scatter Plot from 'Indometh' data set. 
g2 <- ggplot(Indometh, aes(x=time, y=conc)) +
      geom_point() + scale_x_log10() + scale_y_log10() +
      facet_wrap(~ Subject, labeller = label_both) +
      labs(
        title = "Faceted Scatter Plot from Indomethacin Concentration",
        x = "time",
        y = "concentration"
      ) + theme_minimal()
g2

# Save the plot to my file.
ggsave(plot = g2, 
       filename = "Question 2 - Faceted Scatter Plot of Indomethacin Concentration.jpeg",
       width = 6,
       height = 4)

getwd()



#3----------------------------------------------------------------
#Construct faceted histograms using the 'diamonds' data set from the 'ggplot2' package.
#Instruction: histograms of price, use 100 bins for each histogram, cut on the row facet,
#y-axis height is different for each facet row, x-axis on a logarithmic scale, informative x-axis title iwth units,
#informative y-axis title.

# Build faceted histograms using diamonds data set from ggplot2 package.
g3 <- ggplot(diamonds, aes(x=price)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  facet_grid(cut ~ clarity, scales="free_y") +
  labs(
    title = "Faceted histograms for Dimond Price",
    x = "price",
    y = "Count"
  ) + theme_minimal()
g3

# Save the plot to my file.
ggsave(plot=g3,
       filename = "Question 3 - diamonds_histograms.jpeg",
       width = 6,
       height = 4
       )

getwd()


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  





