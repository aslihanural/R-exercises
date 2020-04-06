x <- c(1, "canada", 3)
x
class(x)
as.character(x)

x<-1:5
y<-as.character(x)
y
class(y)
as.numeric(y)

# The function as.character() turns numbers into characters
# The function as.numeric() turns characters into numbers
# In R, missing data is assigned the value NA