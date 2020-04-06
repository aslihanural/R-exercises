# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# determining that the murders dataset is of the "data frame" class
class(murders)

# finding out more about the structure of the object
str(murders)

# showing the first 6 lines of the dataset
head(murders)
class(murders)

murders$population

murders$total

murders$state

# displaying the variable names in the murders dataset
names(murders)

# using the accessor operator to obtain the population column
pop<-murders$population

# determining how many entries are in a vector
length(pop)

tot<-murders$total
length(tot)

# vectors can be of class numeric and character
class(pop)
class(tot)
class(murders$state)

# logical vectors are either TRUE or FALSE
z <- 3 == 2
z
class(z)

# factors are another type of class
class(murders$region)

# obtaining the levels of a factor
levels(murders$region)
