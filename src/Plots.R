## This function creates a grid of plots with a line of best fit. Each plot is an x attribute against SalePrice
plotRelationship <- function(model_ds) {
  data2 <- melt(model_ds[,1:length(model_ds)], id.vars = 'SalePrice')
  
  ggplot(data2) +
    geom_point(aes(value,SalePrice, colour=variable), size = 1) +
    geom_smooth(aes(value,SalePrice, colour=variable), method=lm, se=FALSE) +
    facet_wrap(~variable, scales="free_x")
}

## This function prints out all the residual analysis plots
## This function takes in a linear model object
printPlots <- function(fit) {
  path = 'plots/'
  #setwd(path)
  n_observations <- nrow(fit$model)
  parameters <- length(fit$coefficients) - 1
  # Barplot standardized residuals
  standardRes <- stdres(fit)
  barplot(height = stdres(fit), names.arg = 1:n_observations,
          main = "Standardized Residuals", xlab = "Index",
          ylab = "Standardized Resid", ylim=c(-3,3))
  #Add cutoff values. Either 2 or 3 can be chosen.
  abline(h=3, col = "Red", lwd=2)
  abline(h=-3, col = "Red", lwd=2)
  
  # Barplot studentized residuals
  studentRes <- studres(fit)
  barplot(height = studres(fit), names.arg = 1:n_observations,
          main = "Studentized Residuals", xlab = "Index",
          ylab = "Studentized Resid", ylim=c(-3,3))
  
  abline(h=3, col = "Red", lwd=2)
  abline(h=-3, col = "Red", lwd=2)
  
  # Barplot R-student residuals
  rstudentRes <- rstudent(fit)
  barplot(height = rstudentRes, names.arg = 1:n_observations,
          main = "R Student Residuals", xlab = "Index",
          ylab = "R Student Resid", ylim=c(-4,4))
  corLevel <- 0.05/(2*n_observations)
  corQt <- qt(corLevel, (n_observations - parameters) - 1, lower.tail=F)
  
  abline(h=corQt , col = "Red", lwd=2)
  abline(h=-corQt , col = "Red", lwd=2)
  
  # hat values plot
  #png(filename = 'Hat values plot.png')
  influenceIndexPlot(fit, vars = "hat")
  # save plot
  #dev.off()
  
  # cook distance plot
  #png(filename = 'Cooks distance plot.png')
  influenceIndexPlot(fit, vars = "Cook")
  #save plot
  #dev.off()
  
  # dfbetas plot
  dfbetasPlots(fit,intercept = TRUE, ask = FALSE)
  
  # normal distribution plot
  #png(filename = 'Normal distribution plot.png')
  #par(mfrow=c(1,2))
  hist(studres(fit), breaks=30, freq=F, col="cornflowerblue",
       cex.axis=1.5, cex.lab=1.5, cex.main=2)
  #qqPlot(fit)
  # save the plot
  #dev.off()
  
  ## residual plots
  # rstudent
  #png(filename = 'rstudent residual plot.png')
  residualPlot(fit, type="rstudent", quadratic=F, col = "dodgerblue",
               pch=16, cex=1, cex.axis=1.5, cex.lab=1.5)
  # save the plot
  #dev.off()
  
  # standardized
  #png(filename = 'standardized residual plot.png')
  residualPlot(fit, type="rstandard", quadratic=F, col = "dodgerblue",
               pch=16, cex=1, cex.axis=1.5, cex.lab=1.5)
  # save the plot
  #dev.off()
}