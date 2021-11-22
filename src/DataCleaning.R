library(ggplot2)
library(tidyverse)
library(reshape2)
library(MASS)
library(car)
# url = 'https://raw.githubusercontent.com/BryantR1/CS-4372-Assignment1/main/train.csv'
# train_ds: The untouched train dataset
train_ds = read.csv("data/train.csv")
# working_ds: Copy of the original data. We will use this for our analysis to leave the original data untouched
working_ds <- data.frame(train_ds)
# test_ds: The testing dataset that we will use to predict the accuracy of our model
test_ds = read.csv("data/test.csv")

# before altering the data lets create a correlation matrix for all the variables
check <- which(sapply(working_ds, is.numeric))
cor_df <- colnames(working_ds)[check]
cor_df <- cor_df[-1]
cor_df <- working_ds[cor_df]
cor_df <- cor(cor_df, use = "complete.obs")
cor_df <- data.frame(cor_df)

# create a function to sort all the values in descending order
sortCor <- function(df) {
  # create a placeholder dataframe to be able to merge the following dataframes
  new <- data.frame(matrix(nrow = length(df)))
  # loop over all the correlation matrix columns
  for (i in names(df)) {
  # get the column that we will sort
  attribute <- df[order(df[[i]], decreasing = TRUE),][i]
  # store the index order of the sorted values
  index <- rownames(attribute)
  # store the ordered values, create a dataframe, rename the column to its respective attribute
  content <- attribute[[1]]
  content <- data.frame(content)
  names(content) <- i
  # append the index and values to the dataframe
  new <- data.frame(new,index,content)
  
  }
  # drop the first column which served as a placeholder for the dataframe
  new <- new[-1]
  # return the dataframe with all of the variables sorted by their correlation with the other variables
  return(new)
}

# after creating the correlation matrix, we will create a new dataframe with
# all of the variables sorted in descending order with their respective variables.
cor_df <- sortCor(cor_df)


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
keep_subclass <- c(20, 30, 40, 60, 70)
working_ds[drop_cols] <- NULL
working_ds <- working_ds[working_ds['Utilities'] == 'AllPub', ]
working_ds <- working_ds[!(working_ds$MSZoning %in% drop_zones), ]
working_ds <- working_ds[(working_ds$MSSubClass %in% keep_subclass), ]
working_ds <- working_ds[(working_ds['SaleCondition'] == 'Normal'),]

