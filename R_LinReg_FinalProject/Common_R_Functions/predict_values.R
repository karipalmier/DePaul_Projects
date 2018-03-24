##################################################################################################################################
#
#  predict_data.R
# 
#  This file performs the prediction of dependent data based on the predicted data and model passed in.  It generates the 
#  predicted value, std error of prediction, prediction interval, and confidence interval, then applies the inverse Box Cox to 
#  all of them except the std error.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

predict_values <- function(inModel, predictData, sigLevel, yLambda, basePath, modelDesc){
  
  # Create output file
  outFileName = paste(basePath, modelDesc, "_PredictVals_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")

  # Perform Prediction
  predSE = predict(inModel, predictData, se.fit=T, level=sigLevel)
  predPredInt = predict(inModel, predictData, interval="prediction", level=sigLevel)
  predConfInt = predict(inModel, predictData, interval="confidence", level=sigLevel)
  
  se.CI <- predSE$se.fit
  se.PI <- sqrt(predSE$se.fit^2 + predSE$residual.scale^2)
  alpha <- qt((1-sigLevel)/2, df = predSE$df)
  
  calcConfIntVals = matrix(NA, nrow=length(predSE$fit), ncol=2)
  calcPredIntVals = matrix(NA, nrow=length(predSE$fit), ncol=2)
  for(i in 1:length(predSE$fit)){
    tempCI = predSE$fit[i] + c(alpha, -alpha) * se.CI
    calcConfIntVals[i,1] = tempCI[1]
    calcConfIntVals[i,2] = tempCI[2]
    
    tempCI = predSE$fit[i] + c(alpha, -alpha) * se.PI
    calcPredIntVals[i,1] = tempCI[1]
    calcPredIntVals[i,2] = tempCI[2]
    
  }
  
  # Transformed prediction values and intervals
  transPred = ((predSE$fit*yLambda)+1)^(1/yLambda)
  transCI = ((calcConfIntVals*yLambda)+1)^(1/yLambda)
  transPI = ((calcPredIntVals*yLambda)+1)^(1/yLambda)
  
  # Print prediction values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Output of Prediction with Standard Error Values:", quote=FALSE)
  print("", quote=FALSE)
  print(predSE, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Prediction with Prediction Interval:", quote=FALSE)
  print("", quote=FALSE)
  print(predPredInt, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Prediction with Confidence Interval:", quote=FALSE)
  print("", quote=FALSE)
  print(predConfInt, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Calculated Prediction Interval:", quote=FALSE)
  print("", quote=FALSE)
  print(calcPredIntVals, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Calculated Confidence Interval:", quote=FALSE)
  print("", quote=FALSE)
  print(calcConfIntVals, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Inverted Y Box Cox Transform:", quote=FALSE)
  print("", quote=FALSE)
  print(transPred, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Inverted CI Box Cox Transform:", quote=FALSE)
  print("", quote=FALSE)
  print(transCI, quote=FALSE)
  print("", quote=FALSE)
  print("Output of Inverted PI Box Cox Transform:", quote=FALSE)
  print("", quote=FALSE)
  print(transPI, quote=FALSE)
  print("", quote=FALSE)
  sink()

  close(outFile)
  closeAllConnections()
 
  return(TRUE)
   
}
