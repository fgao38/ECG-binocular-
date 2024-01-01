#Installing packages
install.packages("tidyverse")
install.packages("papaja")
install.packages("ggplot2")
library(tidyverse)
library(papaja)
library(ggplot2)

#Numeric value
Number = 5

#String value
String = "Hi"

#Creating a dataframe
my_dict <- list(tom = 1, david = 2, sam = 3)
my_df <- data.frame(names = names(my_dict), values = unlist(my_dict))