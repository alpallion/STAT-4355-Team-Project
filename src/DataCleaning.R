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

#columns to drop
# 1. get houses with all utilities: Utilities == AllPub
# 2. Consider only residential zones: MSZoning != A, C, FV, I
# Consider different  MSSubclass: Start with 1 story all ages, 1 story w/finished attic all ages,
#     : MSSubClass == 20, 30, 40
# 3. consider only normal house sales: SaleCondition == Normal
drop_zones <- c('A','C (all)', 'FV', 'I')
keep_subclass <- c(20, 30, 40)
working_ds <- working_ds[working_ds['Utilities'] == 'AllPub', ]
working_ds <- working_ds[!(working_ds$MSZoning %in% drop_zones), ]
working_ds <- working_ds[(working_ds$MSSubClass %in% keep_subclass), ]
# for 1 story houses, the general living area and the 1st floor square feet are the same
working_ds$GrLivArea <- NULL
# data still had some values with square feet on the second floor even after filtering for 1 story houses
# remove them
working_ds <- working_ds[working_ds$X2ndFlrSF == 0,]
working_ds <- working_ds[(working_ds['SaleCondition'] == 'Normal'),]


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

## We will create a model based on the first 8 attributes of the
## sale price column of the sorted correlation matrix

model_1_df <- cor_df[,((ncol(cor_df) - 1):ncol(cor_df))]
model_1_attr <- model_1_df[[1]][1:9]
model_1_df <- data.frame(working_ds[model_1_attr])


# Next we will create a model based on attributes that make sense logically
working_ds <- data.frame(train_ds)
#columns to drop
# 1. get houses with all utilities: Utilities == AllPub
# 2. Consider only residential zones: MSZoning != A, C, FV, I
# Consider different  MSSubclass: Start with 1 story all ages, 1 story w/finished attic all ages,
#     : MSSubClass == 20, 30, 40
# 3. consider only normal house sales: SaleCondition == Normal
drop_zones <- c('A','C (all)', 'FV', 'I')
keep_subclass <- c(20, 30, 40)
working_ds <- working_ds[working_ds['Utilities'] == 'AllPub', ]
working_ds <- working_ds[!(working_ds$MSZoning %in% drop_zones), ]
working_ds <- working_ds[(working_ds$MSSubClass %in% keep_subclass), ]
# for 1 story houses, the general living area and the 1st floor square feet are the same
working_ds$GrLivArea <- NULL
# data still had some values with square feet on the second floor even after filtering for 1 story houses
# remove them
working_ds <- working_ds[working_ds$X2ndFlrSF == 0,]
working_ds <- working_ds[(working_ds['SaleCondition'] == 'Normal'),]

model_2_attr <- c('LotArea', 'YearBuilt', 'TotalBsmtSF', 'X1stFlrSF', 'BsmtFullBath', 'BsmtHalfBath',
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars', 'GarageArea', 'SalePrice')

model_2_df <- data.frame(working_ds[model_2_attr])

# Next we will clean the testing dataset to estimate the values of the houses
working_test_ds_2 <- data.frame(test_ds)
#columns to drop
# 1. get houses with all utilities: Utilities == AllPub
# 2. Consider only residential zones: MSZoning != A, C, FV, I
# Consider different  MSSubclass: Start with 1 story all ages, 1 story w/finished attic all ages,
#     : MSSubClass == 20, 30, 40
# 3. consider only normal house sales: SaleCondition == Normal
drop_zones <- c('A','C (all)', 'FV', 'I')
keep_subclass <- c(20, 30, 40)
working_test_ds_2 <- working_test_ds_2[working_test_ds_2['Utilities'] == 'AllPub', ]
working_test_ds_2 <- working_test_ds_2[!(working_test_ds_2$MSZoning %in% drop_zones), ]
working_test_ds_2 <- working_test_ds_2[(working_test_ds_2$MSSubClass %in% keep_subclass), ]
# for 1 story houses, the general living area and the 1st floor square feet are the same
working_test_ds_2$GrLivArea <- NULL
# data still had some values with square feet on the second floor even after filtering for 1 story houses
# remove them
working_test_ds_2 <- working_test_ds_2[working_test_ds_2$X2ndFlrSF == 0,]
working_test_ds_2 <- working_test_ds_2[(working_test_ds_2['SaleCondition'] == 'Normal'),]

model_2_attr <- c('LotArea', 'YearBuilt', 'TotalBsmtSF', 'X1stFlrSF', 'BsmtFullBath', 'BsmtHalfBath',
                  'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars', 'GarageArea')

test_model_2_df <- data.frame(working_test_ds_2[model_2_attr])
test_model_2_df <- na.omit(test_model_2_df)
