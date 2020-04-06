# Read a csv file, named "mtcars.csv"

install.packages("tidyverse")
install.packages("tibble")

install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library("readxl")

# Note that there are quotation marks when installing a package, but not when loading it
# and remember that hashtags let you add useful notes to your code! 

setwd("C:/Users/aslý/Documents")

# xlsx files
my_data_1<- read_excel("Weil_1.xlsx")

my_data_2 <- read_excel("Weil_2.xlsx")

head(my_data_1)    # Displays the first few rows
tail(my_data_1)    # Displays the last rows
str(my_data_1)     # Tells you whether the variables are continuous, integers, categorical or characters

head(my_data_1$Temp)  # Displays the first few rows of this column only
class(my_data_1$Temp) # Tells you what type of variable we're dealing with: it's character now but we want it to be a factor

my_data_1$Temp <- as.numeric(my_data_1$Temp)
class(my_data_1$Temp)
my_data_1$pH <- as.numeric(my_data_1$pH)
class(my_data_1$pH)
my_data_1$TSS <- as.numeric(my_data_1$TSS)
class(my_data_1$TSS)

dim(my_data_1)           # Displays number of rows and columns
summary(my_data_1)       # Gives you a summary of the data
summary(my_data_1$Temp)  # Gives you a summary of that particular variable (column) in your dataset

a <-length(my_data_1)
a
b <-length(my_data_2)
b

plot(my_data_1$Date_time, my_data_1$Temp, xlab = "date", ylab = "Temp", main = "Temp vs. date", pch = 19, cex = 0.5, col = "red")
plot(my_data_1$Date_time, my_data_1$pH, xlab = "date", ylab = "pH", main = "pH vs. date", pch = 19, cex = 0.5, col = "blue")
plot(my_data_1$Date_time, my_data_1$DO, xlab = "date", ylab = "DO", main = "DO vs. date", pch = 19, cex = 0.5, col = "grey")
plot(my_data_1$Date_time, my_data_1$Nitrate_N, xlab = "date", ylab = "Nitrate_N", main = "Nitrate_N vs. date", pch = 19, cex = 0.5, col = "green")
plot(my_data_1$Date_time, my_data_1$TN, xlab = "date", ylab = "TN", main = "TN vs. date", pch = 19, cex = 0.5, col = "pink")
plot(my_data_1$Date_time, my_data_1$TP, xlab = "date", ylab = "TP", main = "TP vs. date", pch = 19, cex = 0.5, col = "purple")
plot(my_data_1$Date_time, my_data_1$TOC, xlab = "date", ylab = "TOC", main = "TOC vs. date", pch = 19, cex = 0.5, col = "black")
plot(my_data_1$Date_time, my_data_1$TSS, xlab = "date", ylab = "TSS", main = "TSS vs. date", pch = 19, cex = 0.5, col = "brown")
plot(my_data_1$TOC, my_data_1$TSS, xlab = "TOC", ylab = "TSS", main = "TSS vs. TOC", pch = 19, cex = 0.5, col = "black")

