##################################################################################################################################
#
#  crossvalidate_data.R
# 
#  This file performs cross validation on the model and data passed in then generates the RMSE, MAPE, MAE, and R2 difference
#  R outputs.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

crossvalidate_data <- function(inModel, inData, percentTest, currentY, varInfo, basePath, modelDesc){
  
  # Create output file
  outFileName = paste(basePath, modelDesc, "_Cross_Validation_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  

  # Calculate the number of points for cross validation
  numFolds = ceiling(100/percentTest)

  
  fileName = paste(basePath, modelDesc, "_CrossValObsPred.jpeg", sep="")
  jpeg(file=fileName)

  # Print cp model selection values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Output of Cross Validation:", quote=FALSE)
  print("", quote=FALSE)
  
  # Perform Cross validation
  crossVal = cv.lm(data=inData, inModel, m=numFolds)
  
  print("", quote=FALSE)
  print(crossVal, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  dev.off()
  
  yPredCv = crossVal$cvpred
  yPredModel = crossVal$Predicted
  yObs = crossVal[,currentY]
  
  # Calculate the RMSE of CV prediction error
  rmseCv = sqrt(sum((yObs - yPredCv)^2)/nrow(inData))
  
  # Calculate the CV Mean Absolute Error
  maeCv = sum(abs(yObs-yPredCv))/nrow(inData)
  
  # Calculate the CV Mean Percentage Absolute Error
  mapeCv = (100/nrow(inData))*sum((abs(yObs - yPredCv))/yObs)
  
  # Calculate the modelRMSE of prediction error
  rmseModel = sqrt(sum((yObs - yPredModel)^2)/nrow(inData))
  
  # Calculate the model Mean Absolute Error
  maeModel = sum(abs(yObs-yPredModel))/nrow(inData)
  
  # Calculate the model Mean Percentage Absolute Error
  mapeModel = (100/nrow(inData))*sum((abs(yObs - yPredModel))/yObs)
  
  r2Cv = cor(cbind(yObs, yPredCv))**2
  r2Cv = r2Cv[1,2]
  r2Model = summary(inModel)$r.squared
  r2Diff = abs(r2Cv - r2Model)
  
  cvParams = vector("list", 8)
  cvParams[[1]] = rmseCv
  cvParams[[2]] = maeCv
  cvParams[[3]] = mapeCv
  cvParams[[4]] = rmseModel
  cvParams[[5]] = maeModel
  cvParams[[6]] = mapeModel
  cvParams[[7]] = r2Cv
  cvParams[[8]] = r2Diff
  
  # Print cp model selection values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print(paste("Number of folds performed: ", numFolds, sep=""), quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Original Model Root Mean Square of Prediction Data (RMSE):", quote=FALSE)
  print("", quote=FALSE)
  print(rmseModel, quote=FALSE)
  print("", quote=FALSE)
  print("Original Model Mean Absolute Error of Predicted Data (MAE):", quote=FALSE)
  print("", quote=FALSE)
  print(maeModel, quote=FALSE)
  print("", quote=FALSE)
  print("Original Model Mean Percentage Absolute Error of Predicted Data (MAPE):", quote=FALSE)
  print("", quote=FALSE)
  print(mapeModel, quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Cross Validation Root Mean Square of Prediction Data (RMSE):", quote=FALSE)
  print("", quote=FALSE)
  print(rmseCv, quote=FALSE)
  print("", quote=FALSE)
  print("Cross Validation Mean Absolute Error of Predicted Data (MAE):", quote=FALSE)
  print("", quote=FALSE)
  print(maeCv, quote=FALSE)
  print("", quote=FALSE)
  print("Cross Validation Mean Percentage Absolute Error of Predicted Data (MAPE):", quote=FALSE)
  print("", quote=FALSE)
  print(mapeCv, quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Cross Validation R-Squared:", quote=FALSE)
  print("", quote=FALSE)
  print(r2Cv, quote=FALSE)
  print("", quote=FALSE)
  print("Model R-Squared:", quote=FALSE)
  print("", quote=FALSE)
  print(r2Model, quote=FALSE)
  print("", quote=FALSE)
  print("Model R-Squared and Cross Validation R-Squared Difference:", quote=FALSE)
  print("", quote=FALSE)
  print(r2Diff, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  crossVal$cvresidual = yObs - yPredCv
  
  fileName = paste(basePath, modelDesc, "_CrossValResVsPred.jpeg", sep="")
  plotTitle = paste("Cross Validation Residuals Vs Predicted")
  jpeg(file=fileName)
  plot(crossVal$Predicted, crossVal$cvresidual, main=plotTitle, xlab="Model Predicted Values", 
       ylab="Cross Validation Prediction Residuals")
  dev.off()
  
  close(outFile)
  closeAllConnections()
  
  return(cvParams)
  
}
