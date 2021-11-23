library(ggplot2)
library(tidyverse)
library(reshape2)
library(MASS)
library(car)

source("src/Plots.R")
source("src/DataCleaning.R")
source("src/DataAnalysis.R")
source("src/influentialPoints.R")
#source("src/testing.R")
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
fit <- lm(SalePrice ~ ., data = model_ds)
parameters <- length(fit$coefficients) - 1
n_observations <- nrow(model_ds)
summary(fit)

# calculating influential points
influenceAnalysis(fit, n_observations)
# printing plots
printPlots(fit)


res_df <- data.frame(standardRes,studentRes,rstudentRes)

#iii)
# The residual barplot shows the residual value for each point. We can see if there are outliers if
# some points go over the threshold that we have set


#anova table
m_anova <- anova(fit)
ss_sum <- sum(m_anova[0:5,2])
res <- m_anova[6,2]
total <- ss_sum + res
reg_df <- 6 - 1
res_df <- m_anova[6,1]
total_df <- 28 - 1

msr <- ss_sum / reg_df
mse <- res / res_df

f_stat <- msr / mse
#construct table
ss_column <- c(ss_sum, res,total)
df_column <- c(reg_df,res_df,total_df)
ms_column <- c(msr,mse,NA)
f_column <- c(f_stat, NA, NA)
anova_t1 <- data.frame(ss_column,df_column,ms_column,f_column)


