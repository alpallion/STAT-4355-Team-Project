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
dummy_vars <- c('OverallQual', 'GarageCars', 'FullBath', 'TotRmsAbvGrd')
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




### Model 2
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
vif(dummy_model_2)
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

