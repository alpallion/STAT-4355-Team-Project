printPlots <- function(fit) {
  path = 'plots/'
  setwd(path)
  
  # barplot standardized residuals
  standardRes <- stdres(fit)
  barplot(height = stdres(fit), names.ag = 1:n_observations,
          main = "Standardized Residuals", xlab = "Index",
          ylab = "Standardized Resid", ylim=c(-3,3))
  #Add cutoff values. Either 2 or 3 can be chosen.
  abline(h=3, col = "Red", lwd=2)
  abline(h=-3, col = "Red", lwd=2)
  
  # barplot studentized residuals
  studentRes <- studres(fit)
  barplot(height = studres(fit), names.ag = 1:n_observations,
          main = "Studentized Residuals", xlab = "Index",
          ylab = "Studentized Resid", ylim=c(-3,3))
  
  abline(h=3, col = "Red", lwd=2)
  abline(h=-3, col = "Red", lwd=2)
  
  # R-student residuals
  rstudentRes <- rstudent(fit)
  barplot(height = rstudentRes, names.ag = 1:n_observations,
          main = "R Student Residuals", xlab = "Index",
          ylab = "R Student Resid", ylim=c(-4,4))
  corLevel <- 0.05/(2*n_observations)
  corQt <- qt(corLevel, (n_observations - parameters) - 1, lower.tail=F)
  
  abline(h=corQt , col = "Red", lwd=2)
  abline(h=-corQt , col = "Red", lwd=2)
  
  # hat values plot
  png(filename = 'Hat values plot.png')
  influenceIndexPlot(fit, vars = "hat")
  dev.off()
  
  # cook distance plot
  png(filename = 'Cooks distance plot.png')
  influenceIndexPlot(fit, vars = "Cook")
  dev.off()
  
  # dfbetas plot
  dfbetasPlots(fit,intercept = TRUE, ask = FALSE)
  
  # normal distribution plot
  png(filename = 'Normal distribution plot.png')
  par(mfrow=c(1,2))
  hist(studres(fit), breaks=10, freq=F, col="cornflowerblue",
       cex.axis=1.5, cex.lab=1.5, cex.main=2)
  qqPlot(fit)
  # save the plot
  dev.off()
  
  ## residual plots
  # rstudent
  png(filename = 'rstudent residual plot.png')
  residualPlot(fit, type="rstudent", quadratic=F, col = "dodgerblue",
               pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
  # save the plot
  dev.off()
  
  # standardized
  png(filename = 'standardized residual plot.png')
  residualPlot(fit, type="rstandard", quadratic=F, col = "dodgerblue",
               pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
  # save the plot
  dev.off()
}
