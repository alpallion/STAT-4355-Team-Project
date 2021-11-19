library(ggplot2)
library(reshape2)
library(MASS)
library(car)
source('ResidualPlots.R')

train_df = read.csv("C:/Users/Bryant/OneDrive/School OneDrive/Senior Year/STAT 4355/Github Project/STAT-4355-Team-Project/data/train.csv")
working_ds <- data.frame(train_df)

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


printResPlots(fit)





