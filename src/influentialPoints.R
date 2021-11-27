influenceAnalysis <- function(fit) {
  # hat values
  hat_vals <- hatvalues(fit)
  # p is the trace of the hat matrix, the sum of all h_ii
  p = sum(hat_vals)
  n = nrow(fit$model)
  hat_vals <- hat_vals[hat_vals > ((2*p)/ n)]
  print(c('The hat values greater than 2*p/n are: ', hat_vals))
  
  # cooks D
  cooks_vals <- cooks.distance(fit)
  # Check for two cutoffs: greater than 1 means definite influential point,
  # greater than 4/n means we should investigate these points
  cutoff_1 <- cooks_vals[cooks_vals > 1]
  cutoff_4n <- cooks_vals[cooks_vals > 4 / n]
  
  if (length(cutoff_1) > 0){
    print(c('The values with cook\'s distance greater than 1 are: ', cutoff_1))
  } else {
    print('There are no cook\'s distance values greater than 1')
  } 
  
  if (length(cutoff_4n) > 0){
    print(c('The values with cook\'s distance greater than 4/n are: ', cutoff_4n))
  } else {
    print('There are no cook\'s distance values greater than 4/n', cutoff_1)
  }
  
  ##TO-DO: Create a graph that will highlight the points classified by the conditions above
  
  # DFBETAS
  betas_vals <- data.frame(dfbetas(fit))
  # create list placeholder
  index_holder <- 0
  for (i in names(betas_vals)){
    # find the points that are greater than 2 / sqrt(n)
    condition <- abs(betas_vals[i]) > 2 /sqrt(n)
    # save the points
    points <- names(condition[condition == TRUE,])
    index_holder <- c(index_holder,list(c(i,points)))
  }
  # remove placeholder
  index_holder[1] <- NULL
  dfbetas_points <- index_holder
  
  if (length(index_holder) > 0){
    print(c('The points that have DFBETAS values greater than 2 / sqrt(n) are: ', index_holder))
  } else {
    print('There are no DFBETAS values that are greater than 2 / sqrt(n)')
  } 
  
  #DFFITS
  dffits_vals <- dffits(fit)
  # number of predictors excluding the intercept
  parameters <- length(fit$coefficients) - 1
  points <- names(dffits_vals[abs(dffits_vals) > 2*sqrt(parameters/n)])
  
  if (length(points) > 0){
    print(c('The points that have DFFITS values greater than 2 * sqrt(p/n) are: ', points))
  } else {
    print('There are no DFFITS values that are greater than 2 * sqrt(p/n)')
  } 
  
  #covratio
  covr_vals <- covratio(fit)
  upper_bound <- 1 + (3*parameters)/n
  lower_bound <- 1 - (3*parameters)/n
  points <- names(covr_vals[(covr_vals < lower_bound) | (covr_vals > upper_bound)])
  
  if (length(points) > 0){
    print(c('The points that pass a COVRATIO boundary of 1 +- (3*p)/n are: ', points))
  } else {
    print('There are no points that pass a COVRATIO boundary')
  } 
  
}


### testing returning the values of the dfbetas analysis

##This function calculates and returns the DFBETAS points for all the attributes used in the linear model
## The function takes in a linear model object
dfbetasPoints <- function(fit){
  n = nrow(fit$model)
  betas_vals <- data.frame(dfbetas(fit))
  # create list placeholder
  index_holder <- 0
  for (i in names(betas_vals)){
    # find the points that are greater than 2 / sqrt(n)
    condition <- abs(betas_vals[i]) > 2 /sqrt(n)
    # save the points
    points <- names(condition[condition == TRUE,])
    # check if there are any DFBETAS values
    if (!is.null(points)){
      index_holder <- c(index_holder,list(c(i,points)))
    }
  }
  # remove placeholder
  index_holder[1] <- NULL
  dfbetas_points <- index_holder
  
  return(dfbetas_points)
}

###This function takes the points calculated by the dfbetasPoints function to create plots
## with the potential influential points highlighted
## This function takes in the dfbetas points and the dataframe used to create the linear model.
# this function works best with models that do not have factors
influentialPointsPlots <- function(dfbetas_points, df) {
  for (i in (2:length(dfbetas_points))) {
    attribute <- dfbetas_points[[i]][1]
    observations <- dfbetas_points[[i]][2:length(dfbetas_points[[i]])]
    observations <- as.integer(observations)
    highlight_df <- df[rownames(df) %in% observations,]
    
    # creating a plot with all the observations of the df and highlighting the
    # observations found by the dfbetas test
    p <- ggplot() + 
      geom_point(aes(x = df[[attribute]], y = df$SalePrice)) +
      geom_point(aes(x = highlight_df[[attribute]], y = highlight_df$SalePrice,  color= 'Inf. Pts'), size=2) +
      labs(x = attribute, y = 'SalePrice')
    
    print(p)
  }
}