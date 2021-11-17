library(ggplot2)
library(tidyverse)
library(reshape2)
library(MASS)
library(car)
# url = 'https://raw.githubusercontent.com/BryantR1/CS-4372-Assignment1/main/train.csv'
train_df = read.csv("data/train.csv")
working_ds <- data.frame(train_df)
test_df = read.csv("data/test.csv")

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

