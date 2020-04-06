library(dslabs)
data(murders)
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order

index <- order(x)    # returns index that will put x in order
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
sort(x)
order(x)
rank(x)    # returns ranks (smallest to largest)

# The name of the state with the maximum population is found by doing the following
murders$state[which.max(murders$population)]
murders$state[which.max(murders$total)]
max(murders$population)
max(murders$total)

# how to obtain the murder rate
murder_rate <- murders$total / murders$population * 100000
murder_rate

# ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing=TRUE)]

# ordering the states by murder rate, from the smallest to the highest
murders$state[order(murder_rate)]

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time/60
V<-distance/(time/60)
V
