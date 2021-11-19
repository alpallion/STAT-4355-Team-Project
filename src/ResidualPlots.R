printResPlots <- function(fit) {
  path = 'plots/'
  setwd(path)
  # residual plots
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
