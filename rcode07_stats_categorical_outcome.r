############################################################################

# just in case, clear the workspace

rm(list=ls())

# read in data

load("data_survey_edit.rdata")

############################################################################

# one-way contingency tables

table(dat$sex)
table(dat$smoke)

# two-way contingency table

table(dat$sex, dat$smoke)

# table with margin totals added

addmargins(table(dat$sex, dat$smoke))

# proportions

table(dat$sex)
prop.table(table(dat$sex))

table(dat$sex, dat$smoke)

prop.table(table(dat$sex, dat$smoke))

# note: proportions are computed out of the total sample size

# add margin proportions

addmargins(prop.table(table(dat$sex, dat$smoke)))

# can also compute proportions over rows (1) or columns (2)

prop.table(table(dat$sex, dat$smoke), margin=1)
prop.table(table(dat$sex, dat$smoke), margin=2)

# add margin proportions

addmargins(prop.table(table(dat$sex, dat$smoke), margin=1), margin=2)
addmargins(prop.table(table(dat$sex, dat$smoke), margin=2), margin=1)

############################################################################

# testing for association between two categorical variables

# chi-square test

chisq.test(dat$sex, dat$smoke)

# can also use contingency table as input

chisq.test(table(dat$sex, dat$smoke))

# Fisher's exact test

fisher.test(dat$sex, dat$smoke)
fisher.test(table(dat$sex, dat$smoke))

############################################################################

# logistic regression

res <- glm(smoke ~ sex, data=dat, family=binomial)

# the outcome variable for logistic regression must be coded 0/1 or can be a
# factor (but then you have to figure out what the reference level is, so you
# know which of the two events is being predicted)

factor(dat$smoke)

res <- glm(factor(smoke) ~ sex, data=dat, family=binomial)
summary(res)

# it may be safer to do the coding manually

dat$smoke1 <- ifelse(dat$smoke == "yes", 1, 0)

res <- glm(smoke1 ~ sex, data=dat, family=binomial)
summary(res)

# get 95% CI for the model coefficients

confint(res)

# back-transform to odds ratios

coef(res)
exp(coef(res))
exp(confint(res))

data.frame(OR = exp(coef(res)), CI = exp(confint(res)))
round(data.frame(OR = exp(coef(res)), CI = exp(confint(res))), 2)

############################################################################

# predictors can be mix of continuous and categorical variables

res <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial)
summary(res)

# a three-way interaction

res <- glm(smoke1 ~ age * sex * pss, data=dat, family=binomial)
summary(res)

# full versus reduced model comparison (likelihood ratio test)

res1 <- glm(smoke1 ~ age * sex * pss, data=dat, family=binomial)
res0 <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial)

anova(res1, res0, test="Chisq")

############################################################################

# ROC curves

# install (if necessary) the 'pROC' package and load it

if (!require(pROC)) install.packages("pROC")

library(pROC)

# fit model (note: set na.action=na.exclude, so that the predict() function
# below will return NA for persons where data are missing)

res <- glm(smoke1 ~ age + sex + pss, data=dat, family=binomial, na.action=na.exclude)
summary(res)

# add predicted probabilities to the data frame

dat$pred <- predict(res, type="response")

# create ROC curve

res <- roc(smoke1 ~ pred, data=dat)
res
plot(res)

# area under the curve

auc(res)

############################################################################
