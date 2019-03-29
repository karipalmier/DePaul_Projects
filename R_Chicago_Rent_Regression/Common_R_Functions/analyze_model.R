##################################################################################################################################
#
#  analyze_model.R
# 
#  This file generates the model summary, ANOVA, standardized coeff, VIF, and influential point R outputs and the std residual vs
#  predicted, std residual normal plot, and std residual vs independent var plots.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

analyze_model <- function(model, inData, modelForm, varInfo, basePath, modelDesc){

  # Create output file
  outFileName = paste(basePath, modelDesc, "_Analysis_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  
  # Create plot path
  dir.create(file.path(basePath, "Analysis Plots"), showWarnings = FALSE)
  plotPath = paste(basePath, "Analysis Plots\\", sep="")
  
  
  modelSummary = summary(model)
  modelAnova = anova(model)
  
  # Print model summary values to output file
  sink(file=outFile, append=TRUE)
  print("Model Formula:", quote=FALSE)
  print("", quote=FALSE)
  print(modelForm)
  print("", quote=FALSE)
  print("Model Summary:", quote=FALSE)
  print("", quote=FALSE)
  print(modelSummary)
  print("", quote=FALSE)
  print("Model ANOVA Analysis:", quote=FALSE)
  print("", quote=FALSE)
  print(modelAnova)
  print("", quote=FALSE)
  sink()
  
  # Calculate Standardized Coefficients and display them and confidence levels
  stdCoeffs = lm.beta(model)
  modelCIs = confint(model, level=0.95)
  
  # Print model stnd coeffs conf int values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Standardized Coefficients:", quote=FALSE)
  print("", quote=FALSE)
  print(stdCoeffs)
  print("", quote=FALSE)
  print("95% Confidence Intervals:", quote=FALSE)
  print("", quote=FALSE)
  print(modelCIs)
  print("", quote=FALSE)
  sink()
  
  
  # Plot Standardized Residuals vs Predicted Values
  fileName = paste(plotPath, modelDesc, "_StndResVsPredVals.jpeg", sep="")
  plotTitle = "Standardized Residuals Versus Predicted Values"
  jpeg(file=fileName)
  plot(fitted(model), rstandard(model), main=plotTitle,
       xlab="Predicted Values", ylab="Standardized Residuals")
  abline(a=0, b=0, col="red")
  abline(a=3, b=0, col="blue")
  abline(a=-3, b=0, col="blue")
  dev.off()
  
  # Plot Standardized Residuals Normal Plot
  fileName = paste(plotPath, modelDesc, "_StndResNormalPlot.jpeg", sep="")
  plotTitle = "Standardized Residuals Normal Plot"
  jpeg(file=fileName)
  qqnorm(rstandard(model), main=plotTitle)
  qqline(rstandard(model), col=2)
  dev.off()
  
  # Plot Standardized Residuals vs x variables
  numXVars = length(varInfo.xVarNames)
  for (i in 1:numXVars){
    var = varInfo.xVarNames[i]
    fileName = paste(plotPath, modelDesc, "_StndResVs", var, ".jpeg", sep="")
    plotTitle = paste("Standard Residuals Versus ", varInfo.xVarPlotNames[i])
    plotData = data.frame(inData[var], rstandard(model))
    jpeg(file=fileName)
    plot(plotData, main=plotTitle,
         xlab=var, ylab="Standardized Residuals")
    abline(a=0, b=0, col="red")
    abline(a=3, b=0, col="blue")
    abline(a=-3, b=0, col="blue")
    dev.off()
  }
  
  
  # Compute the model VIF values
  modelVIF = vif(model)

  # Print model VIF values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Model VIF Values:", quote=FALSE)
  print("", quote=FALSE)
  print(modelVIF, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  # Get influence point information
  inflMeas = influence.measures(model)
  sumInflMeas = summary(inflMeas);
  dfBetaVal = dfbeta(model)
  covRatVal = covratio(model)
  dfFitVal = dffits(model)
  cookDVal = cooks.distance(model)
  hatVals = hatvalues(model)
  delStudRes = rstudent(model)
  
  hatInfPts = (hatVals >= 0.5) & (abs(delStudRes) > 3)
  hatInfNdx = which(hatInfPts %in% TRUE)
  
  cookDPts = (cookDVal >= 1)
  cookDNdx = which(cookDPts %in% TRUE)
  
  combInfNdx = append(hatInfNdx, cookDNdx)
  uniqInfNdx = unique(combInfNdx)
  
  # Print model influence point values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Potential Influential Points:", quote=FALSE)
  summary(inflMeas)
  print("", quote=FALSE)
  sink()

  # Plot Studentized Residuals vs Hat Values
  fileName = paste(plotPath, modelDesc, "_DelStudResVsHatVals.jpeg", sep="")
  plotTitle = "Deleted Studentized Residuals Versus Hat Values"
  jpeg(file=fileName)
  plot(delStudRes~hatVals, main=plotTitle,
       xlab="Hat Values", ylab="Deleted Studentized Residuals")
  abline(a=0, b=0, col="red")
  abline(a=3, b=0, col="blue")
  abline(a=-3, b=0, col="blue")
  dev.off()
  
  
  close(outFile)
  closeAllConnections()
  
  return(uniqInfNdx)
  
}





