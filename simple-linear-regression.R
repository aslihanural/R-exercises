set.seed(1453)

N <- 100 ## no. of observations

weight <- runif(n=N, min=60, max=100) ## hypothetical weights in kg

height <- 2.2*weight + rnorm(n=N, mean=0, sd=10) ## hypothetical heights in cm

plot(weight, height, pch=19, xlab='Weight (kg)', ylab='Height (cm)', col='grey')

plot(weight, height, pch=20, xlab='Weight (kg)', ylab='Height (cm)', col='red')

?lm()

## Linear model fit
fit <- lm(height ~ weight)

## Plot model fit
plot(weight, height, pch=19, xlab='Weight (kg)', ylab='Height (cm)', col='grey')

lines(weight, predict(fit), col='black', lwd=3)

## Print result
print(fit)

## Create data frame
df <- data.frame(height=height, weight=weight)
head(df)

## Fit linear model
fit <- lm(height ~ weight, data=df)

summary(fit)

confint(fit, level=0.97) ## pick the 97% confidence intervals

hist(fit$residuals)
