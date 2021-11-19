library(ggplot2)
library(reshape2)
library(MASS)
library(car)
# url = 'https://raw.githubusercontent.com/BryantR1/CS-4372-Assignment1/main/train.csv'
train_df <- read_csv("Downloads/train.csv")
working_ds <- data.frame(train_df)
test_df <- read_csv("Downloads/test.csv")

#columns to drop
# 1. get houses with all utilities: Utilities == AllPub
# 2. Consider only residential zones: MSZoning != A, C, FV, I
# Consider different  MSSubclass: Start with 1 story all ages, 1 story w/finished attic all ages,
#   2 story all ages: MSSubClass == 20, 30, 40, 60, 70
# 3. check condition1 and condition2, optimally we would want all houses to have 'normal' conditions
#     as a nearby railroad or park may affect the price
# 4. Consider GarageFinish as Finished only
# 5. consider only normal house sales: SaleCondition == Normal

drop_cols <- c('Street', 'Alley', 'KitchenAbvGr', 'KitchenQual')
drop_basement <- c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinSF1', 'BsmtFinType2', 'BsmtFinSF2', 'BsmtUnfSF')
drop_cols <- c(drop_cols, drop_basement)
drop_zones <- c('A','C (all)', 'FV', 'I')
keep_suubclass <- c(20, 30, 40, 60, 70)
working_ds[drop_cols] <- NULL
working_ds <- working_ds[working_ds['Utilities'] == 'AllPub', ]
working_ds <- working_ds[!(working_ds$MSZoning %in% drop_zones), ]
working_ds <- working_ds[(working_ds$MSSubClass %in% keep_suubclass), ]
working_ds <- working_ds[(working_ds['SaleCondition'] == 'Normal'),]


## creating a linear model
model_attr <- c('LotArea', 'YearBuilt', 'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath',
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars', 'GarageArea', 'SalePrice')
factor_drop <- c('BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars')

model_ds <- data.frame(working_ds[model_attr])

# plotting relationship between variables
data2 <- melt(model_ds[,1:15], id.vars = 'SalePrice')

ggplot(data2) +
  geom_jitter(aes(value,SalePrice, colour=variable),) +
  geom_smooth(aes(value,SalePrice, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x")

# converting discrete attributes into factors
#model_ds$bsmtFullBathFactor <- factor(model_ds$BsmtFullBath)
#model_ds$bsmtHalfBathFactor <- factor(model_ds$BsmtHalfBath)
#model_ds$fullBathFactor <- factor(model_ds$FullBath)
#model_ds$halfBathFactor <- factor(model_ds$HalfBath)
#model_ds$bedroomFactor <- factor(model_ds$BedroomAbvGr)
#model_ds$fireplacesFactor <- factor(model_ds$Fireplaces)
#model_ds$carsFactor <- factor(model_ds$GarageCars)

# removing the discrete attribute columns
#model_ds[factor_drop] <- NULL

# creating a linear model
fit <- lm(SalePrice ~., model_ds)
summary(fit)


# standardized residuals
standardRes <- stdres(fit)
print(standardRes)
range(stdres(fit))

barplot(height = stdres(fit), names.ag = 1:nrow(model_ds),
        main = "Standardized Residuals", xlab = "Index",
        ylab = "Standardized Resid", ylim=c(-3,3))
#Add cutoff values. Either 2 or 3 can be chosen.
abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)


# studentized residuals
studentRes <- studres(fit)
print(studentRes)

range(studres(fit))

barplot(height = studres(fit), names.ag = 1:nrow(model_ds),
        main = "Studentized Residuals", xlab = "Index",
        ylab = "Studentized Resid", ylim=c(-3,3))

abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)

# R-student residuals
rstudentRes <- rstudent(fit)
print(rstudentRes)
range(rstudentRes)

barplot(height = rstudentRes, names.ag = 1:nrow(model_ds),
        main = "R Student Residuals", xlab = "Index",
        ylab = "R Student Resid", ylim=c(-4,4))

corLevel <- 0.05/(2*25)
corQt <- qt(corLevel, 21, lower.tail=F)
rstudentRes > corQt

abline(h=corQt , col = "Red", lwd=2)
abline(h=-corQt , col = "Red", lwd=2)

res_df <- data.frame(standardRes,studentRes,rstudentRes)

#iii)
# The residual barplot shows the residual value for each point. We can see if there are outliers if
# some points go over the threshold that we have set

#b)
# hat values
hat_vals <- hatvalues(fit)
p = sum(hat_vals)
n = nrow(data)
hat_vals[hat_vals > ((2*p)/ n)]
# potential influential points are 18 and 27 since they are greater than the threshold 2p/n

# cooks D
cooks_vals <- cooks.distance(fit)
cooks_vals[cooks_vals > 1]
# Since none of the cook's distance values are greater than 1, there is no potential influential
# points according to the cook's distance method.


# DFBETAS
betas_vals <- dfbetas(fit)
abs(betas_vals) > 2 /sqrt(n)
which(betas_vals > 2 /sqrt(n), arr.ind = TRUE)
# point 10 is a potential influential point for beta_8
# point 21 is a potential influential point for beta_7


#DFFITS
dffits_vals <- dffits(fit)
dffits_vals[abs(dffits_vals) > 2*sqrt(p/n)]
# Since none of the DFFITS values are greater than 2 * sqrt(4/28), there is no potential influential
# points according to the DFFITS method.

#covratio
covr_vals <- covratio(fit)
# check if lower bound is appropriate for this model
n > 3*p
covr_vals[(covr_vals < 1 - (3*p)/n) | (covr_vals > 1 + (3*p)/n)]

#The points 1, 5, 11, 17, 18, 27 are considered influential by the covratio method

myInf <- influence.measures(fit)
summary(myInf)


#ii)
influenceIndexPlot(fit, vars = c("hat", "Cook"))
dfbetasPlots(fit,intercept=T)

# normal distribution plot
par(mfrow=c(1,2))
hist(studres(fit), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(fit)


# residual plots
par(mfrow=c(1,1))
# rstudent
residualPlot(fit, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

# standardized
residualPlot(fit, type="rstandard", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

#Setting each of the predictor variables to xi where i = 1,2,...,14
#And the response variable to y
y <- model_ds[, 15]
x1 <- model_ds[, 1]
x2 <- model_ds[, 2]
x3 <- model_ds[, 3]
x4 <- model_ds[, 4]
x5 <- model_ds[, 5]
x6 <- model_ds[, 6]
x7 <- model_ds[, 7]
x8 <- model_ds[, 8]
x9 <- model_ds[, 9]
x10 <- model_ds[, 10]
x11 <- model_ds[, 11]
x12 <- model_ds[, 12]
x13 <- model_ds[, 13]
x14 <- model_ds[, 14]
testfit1 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(testfit1)
qf(0.05, 14, 748, lower.tail = FALSE)
#based on T-test at 90% and 95% Confidence
#x6,x8,x14
#Based on P-value at 90% Confidence
#x6,x8,x14
#Based on P-Value at 95% Confidence
#x4,x6,x8,x12,x14