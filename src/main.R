library(ggplot2)
library(tidyverse)
library(reshape2)
library(MASS)
library(car)
library(fastDummies)

source("src/Plots.R")
source("src/DataCleaning.R")
source("src/DataAnalysis.R")
source("src/influentialPoints.R")
#source("src/testing.R")

### Model 1
# plotting relationship between variables
plotRelationship(model_1_df)
# show the potential outliers in the model before creating dummy variables
model_1_lm <- lm(SalePrice ~ ., data = model_1_df)
summary(model_1_lm)
# store the DFBETAS points
dfbetas_model_1_lm <- dfbetasPoints(model_1_lm)
# Plot the influential points
influentialPointsPlots(dfbetas_model_1_lm, model_1_df)

## On the relationship plot, we can see we need to convert some variables into factors to get an accurate linear model
# get the name of the columns that we want to convert to factors
dummy_vars <- c('OverallQual', 'TotRmsAbvGrd', 'FullBath', 'GarageCars')
dummy_df_1 <- dummy_cols(model_1_df, select_columns = dummy_vars, remove_selected_columns = TRUE)

# create a linear model using the new dataframe 
dummy_model_1 <- lm(SalePrice ~ ., data = dummy_df_1)
summary(dummy_model_1)
#check for vif values
#vif(dummy_model_1)
## we can see that we cannot calculate the VIFs because some of the variables are linearly dependent
## we will remove these variables and create a new linear model
colinearity_attr <- attributes(alias(dummy_model_1)$Complete)$dimnames[[1]]
dummy_df_1 <- dummy_df_1[!(names(dummy_df_1) %in% colinearity_attr)]
# creating a new linear model without the variables that were linearly dependent
dummy_model_1 <- lm(SalePrice ~ ., data = dummy_df_1)
summary(dummy_model_1)
# check the vif values
vif(dummy_model_1)
## now that we can calculate the vif values we will plot them to see which ones are significant
## A guideline is if the VIF value is greater than 5 it is of concern, if it is greater than 10 is is problematic
model_1_vifs <- data.frame(vif(dummy_model_1))
model_1_vifs$variables <- rownames(model_1_vifs)
names(model_1_vifs) <- c('vif', 'variable')

ggplot(model_1_vifs) +
  geom_bar(aes(x = variable, y = vif, fill = variable), stat = "identity") +
  geom_text(aes(x = variable, y=vif, label = sprintf("%.1f", vif)), vjust=0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))

# calculating influential points
influenceAnalysis(dummy_model_1)

# store the DFBETAS points
dfbetas_model_1 <- dfbetasPoints(dummy_model_1)

# Plot the influential points
influentialPointsPlots(dfbetas_model_1, dummy_df_1)

# printing residual plots
printPlots(dummy_model_1)
## From the residual plots we can see that there is a slight cone shape.
## We will use a square root transformation
# make a copy of the dataframe from our initial model
dummy_df_1_transform <- data.frame(dummy_df_1)
# take the square root of the response variable, in this case SalePrice
dummy_df_1_transform$SalePrice <- sqrt(dummy_df_1_transform$SalePrice)
dummy_model_1_transform <- lm(SalePrice ~ ., data = dummy_df_1_transform)
summary(dummy_model_1_transform)
## once again create new plots for the transformed model
# store the DFBETAS points
dfbetas_model_1_transform <- dfbetasPoints(dummy_model_1_transform)

# Plot the influential points
influentialPointsPlots(dfbetas_model_1_transform, dummy_df_1_transform)

# printing residual plots
printPlots(dummy_model_1_transform)

#############
##Model 1 without outliers
#############
temp <- dfbetas_model_1_lm[[9]][2:length(dfbetas_model_1_lm[[9]])]
temp_df <- model_1_df[rownames(model_1_df) %in% temp,]

#outliers for 1st floor
outliers_1 <- c(441,1224)
#outliers for basement
outliers_2 <- c(333,935,1224,441)
# outliers for fullbath
outliers_3 <- c(54,441)
# outliers for garage area
outliers_4 <- c(441)
remove_outliers <- c(outliers_1,outliers_2,outliers_3,outliers_4)
remove_outliers <- unique(remove_outliers)

# create a new dataframe with the outlier observations removed
model_1_df_outliers <- data.frame(model_1_df)
model_1_df_outliers <- model_1_df_outliers[!rownames(model_1_df_outliers) %in% remove_outliers,]
# plot the relationship after removing the outliers
plotRelationship(model_1_df_outliers)
# create a linear model
model_1_lm_outliers <- lm(SalePrice ~ ., data = model_1_df_outliers)
summary(model_1_lm_outliers)

## once again create new plots for the transformed model
# store the DFBETAS points
dfbetas_model_1_outliers <- dfbetasPoints(model_1_lm_outliers)

# Plot the influential points
influentialPointsPlots(dfbetas_model_1_outliers, model_1_df_outliers)

## get the name of the columns that we want to convert to factors
dummy_vars <- c('OverallQual', 'TotRmsAbvGrd', 'FullBath', 'GarageCars' )
dummy_df_1_outliers <- dummy_cols(model_1_df_outliers, select_columns = dummy_vars, remove_selected_columns = TRUE)

# create a linear model using the new dataframe 
dummy_model_1_outliers <- lm(SalePrice ~ ., data = dummy_df_1_outliers)
summary(dummy_model_1_outliers)
# remove collinearity
colinearity_attr <- attributes(alias(dummy_model_1_outliers)$Complete)$dimnames[[1]]
dummy_df_1_outliers <- dummy_df_1_outliers[!(names(dummy_df_1_outliers) %in% colinearity_attr)]
# creating a new linear model without the variables that were linearly dependent
dummy_model_1_outliers <- lm(SalePrice ~ ., data = dummy_df_1_outliers)
summary(dummy_model_1_outliers)



#############
##Model 2
#############
# plotting relationship between variables
plotRelationship(model_2_df)
# show the potential outliers in the model before creating dummy variables
model_2_lm <- lm(SalePrice ~ ., data = model_2_df)
summary(model_2_lm)
# store the DFBETAS points
dfbetas_model_2_lm <- dfbetasPoints(model_2_lm)

# Plot the influential points
influentialPointsPlots(dfbetas_model_2_lm, model_2_df)

## On the relationship plot, we can see we need to convert some variables into factors to get an accurate linear model
# get the name of the columns that we want to convert to factors
dummy_vars <- c('BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars')
dummy_df_2 <- dummy_cols(model_2_df, select_columns = dummy_vars, remove_selected_columns = TRUE)

# create a linear model using the new dataframe 
dummy_model_2 <- lm(SalePrice ~ ., data = dummy_df_2)
summary(dummy_model_2)
#check for vif values
#vif(dummy_model_2)
## we can see that we cannot calculate the VIFs because some of the variables are linearly dependent
## we will remove these variables and create a new linear model
colinearity_attr <- attributes(alias(dummy_model_2)$Complete)$dimnames[[1]]
dummy_df_2 <- dummy_df_2[!(names(dummy_df_2) %in% colinearity_attr)]
# creating a new linear model without the variables that were linearly dependent
dummy_model_2 <- lm(SalePrice ~ ., data = dummy_df_2)
summary(dummy_model_2)
# check the vif values
vif(dummy_model_2)
## now that we can calculate the vif values we will plot them to see which ones are significant
## A guideline is if the VIF value is greater than 5 it is of concern, if it is greater than 10 is is problematic
model_2_vifs <- data.frame(vif(dummy_model_2))
model_2_vifs$variables <- rownames(model_2_vifs)
names(model_2_vifs) <- c('vif', 'variable')

ggplot(model_2_vifs) +
  geom_bar(aes(x = variable, y = vif, fill = variable), stat = "identity") +
  geom_text(aes(x = variable, y=vif, label = sprintf("%.1f", vif)), vjust=0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))

# calculating influential points
influenceAnalysis(dummy_model_2)

# store the DFBETAS points
dfbetas_model_2 <- dfbetasPoints(dummy_model_2)

# Plot the influential points
influentialPointsPlots(dfbetas_model_2, dummy_df_2)

# printing residual plots
printPlots(dummy_model_2)
## From the residual plots we can see that there is a slight cone shape.
## We will use a square root transformation
# make a copy of the dataframe from our initial model
dummy_df_2_transform <- data.frame(dummy_df_2)
# take the square root of the response variable, in this case SalePrice
dummy_df_2_transform$SalePrice <- sqrt(dummy_df_2_transform$SalePrice)
dummy_model_2_transform <- lm(SalePrice ~ ., data = dummy_df_2_transform)
summary(dummy_model_2_transform)
## once again create new plots for the transformed model
# store the DFBETAS points
dfbetas_model_2_transform <- dfbetasPoints(dummy_model_2_transform)

# Plot the influential points
influentialPointsPlots(dfbetas_model_2_transform, dummy_df_2_transform)

# printing residual plots
printPlots(dummy_model_2_transform)


#############
##Model 2 without outliers
#############
temp <- dfbetas_model_2_lm[[2]][2:length(dfbetas_model_2_lm[[2]])]
temp_df <- model_2_df[rownames(model_2_df) %in% temp,]

#outliers for lot area
outliers_1 <- c(54,314,1397,452,707,458,272,1288,1185,1447,1384,877)
#outliers for basement
outliers_2 <- c(935,1224,333, 441)
# outliers for 1st floor
outliers_3 <- c(1224,441)
# outliers for fullbath
outliers_4 <- c(54)
# outliers for bedrooms
outliers_5 <- c(54)
# outliers for garage area
outliers_6 <- c(441)
remove_outliers <- c(outliers_1,outliers_2,outliers_3,outliers_4,outliers_5,outliers_6)
remove_outliers <- unique(remove_outliers)
# create a new dataframe with the outlier observations removed
model_2_df_outliers <- data.frame(model_2_df)
model_2_df_outliers <- model_2_df_outliers[!rownames(model_2_df_outliers) %in% remove_outliers,]
# plot the relationship after removing the outliers
plotRelationship(model_2_df_outliers)
# create a linear model
model_2_lm_outliers <- lm(SalePrice ~ ., data = model_2_df_outliers)
summary(model_2_lm_outliers)
## once again create new plots for the transformed model
# store the DFBETAS points
dfbetas_model_2_outliers <- dfbetasPoints(model_2_lm_outliers)

# Plot the influential points
influentialPointsPlots(dfbetas_model_2_outliers, model_2_df_outliers)

## get the name of the columns that we want to convert to factors
dummy_vars <- c('BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars')
dummy_df_2_outliers <- dummy_cols(model_2_df_outliers, select_columns = dummy_vars, remove_selected_columns = TRUE)

# create a linear model using the new dataframe 
dummy_model_2_outliers <- lm(SalePrice ~ ., data = dummy_df_2_outliers)
summary(dummy_model_2_outliers)
# remove collinearity
colinearity_attr <- attributes(alias(dummy_model_2_outliers)$Complete)$dimnames[[1]]
dummy_df_2_outliers <- dummy_df_2_outliers[!(names(dummy_df_2_outliers) %in% colinearity_attr)]
# creating a new linear model without the variables that were linearly dependent
dummy_model_2_outliers <- lm(SalePrice ~ ., data = dummy_df_2_outliers)
summary(dummy_model_2_outliers)

### using the previous model to predict the price of houses in the testing dataset
dummy_vars <- c('BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr', 'Fireplaces', 'GarageCars')
dummy_df_test_2 <- dummy_cols(test_model_2_df, select_columns = dummy_vars, remove_selected_columns = TRUE)
# attributes of the outlier linear model
keep <- names(dummy_model_2_outliers$coefficients)[2:length(dummy_model_2_outliers$coefficients)]
dummy_df_test_2 <- dummy_df_test_2[names(dummy_df_test_2) %in% keep]
prediction <- predict(dummy_model_2_outliers, newdata = dummy_df_test_2)
# store predicted house prices in the cleaned testing dataframe
test_model_2_df$SalePrice <- prediction
# plot the relationship between all variables and predicted sale price
plotRelationship(test_model_2_df)

