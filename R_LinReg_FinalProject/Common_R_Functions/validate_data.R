##################################################################################################################################
#
#  validate_data.R
# 
#  This file performs validation on the model and training and testing data passed in then generates the RMSE, MAPE, MAE, and R2 
#  difference R outputs.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

validate_data <- function(inModel, trainData, testData, currentY, percentTest, validateTest, varInfo, basePath, modelDesc){
  
  # Create output file
  outFileName = paste(basePath, modelDesc, "_Validation_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  

   # Get Training Set Predicted Data
  yPredTrain = fitted(inModel)
  yObsTrain = trainData[, currentY]
  
  # Calculate the Training Set RMSE of prediction error
  rmseTrain = sqrt(sum((yObsTrain - yPredTrain)^2)/nrow(trainData))
  
  # Calculate Training Set Mean Absolute Error
  maeTrain = sum(abs(yObsTrain-yPredTrain))/nrow(trainData)
  
  # Calculate Training Set Mean Percentage Absolute ERror
  mapeTrain = (100/nrow(trainData))*sum((abs(yObsTrain - yPredTrain))/yObsTrain)
  

  valParams = vector("list", 8)
  valParams[[1]] = rmseTrain
  valParams[[2]] = maeTrain
  valParams[[3]] = mapeTrain
  
  if(validateTest == TRUE){
    # Get Testing Set Predicted Data
    yPredTest = predict(inModel, testData)
    yObsTest = testData[, currentY]
    
    # Calculate the Testing Set RMSE of prediction error
    rmseTest = sqrt(sum((yObsTest - yPredTest)^2)/nrow(testData))
    
    # Calculate Testing Set Mean Absolute Error
    maeTest = sum(abs(yObsTest-yPredTest))/nrow(testData)
    
    # Calculate Testing Set Mean Percentage Absolute ERror
    mapeTest = (100/nrow(testData))*sum((abs(yObsTest - yPredTest))/yObsTest)
    
    r2Test = cor(cbind(yObsTest, yPredTest))**2
    r2Test = r2Test[1,2]
    r2Train = summary(inModel)$r.squared
    r2Diff = abs(r2Test - r2Train)
    
    valParams[[4]] = rmseTest
    valParams[[5]] = maeTest
    valParams[[6]] = mapeTest
    
    valParams[[7]] = r2Test
    valParams[[8]] = r2Diff
    
  }else{
    valParams[[4]] = NA
    valParams[[5]] = NA
    valParams[[6]] = NA
    
    valParams[[7]] = NA
    valParams[[8]] = NA
    
  }
  
  
  # Calculate Testing Set validated R^2 and Testing Set validated/training R^2 difference
  if(validateTest == TRUE){
  }
  
  
  # Print cp model selection values to output file
  sink(file=outFile, append=TRUE)
  print(paste("Training Set Percentage Of Original Data: ", (100-percentTest), " %, Testing Set Percentage of Original Data: ", percentTest,
              " %", sep=""), quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Training Set Root Mean Square of Prediction Data (RMSE):", quote=FALSE)
  print("", quote=FALSE)
  print(rmseTrain, quote=FALSE)
  print("", quote=FALSE)
  print("Training Set Mean Absolute Error of Predicted Data (MAE):", quote=FALSE)
  print("", quote=FALSE)
  print(maeTrain, quote=FALSE)
  print("", quote=FALSE)
  print("Training Set Mean Percentage Absolute Error of Predicted Data (MAPE):", quote=FALSE)
  print("", quote=FALSE)
  print(mapeTrain, quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  if(validateTest == TRUE){
    print("Testing Set Root Mean Square of Prediction Data (RMSE):", quote=FALSE)
    print("", quote=FALSE)
    print(rmseTest, quote=FALSE)
    print("", quote=FALSE)
    print("Testing Set Mean Absolute Error of Predicted Data (MAE):", quote=FALSE)
    print("", quote=FALSE)
    print(maeTest, quote=FALSE)
    print("", quote=FALSE)
    print("Testing Set Mean Percentage Absolute Error of Predicted Data (MAPE):", quote=FALSE)
    print("", quote=FALSE)
    print(mapeTest, quote=FALSE)
    print("", quote=FALSE)
    print("", quote=FALSE)
    print("Testing Set R-Squared:", quote=FALSE)
    print("", quote=FALSE)
    print(r2Test, quote=FALSE)
    print("", quote=FALSE)
    print("Training Set Model R-Squared:", quote=FALSE)
    print("", quote=FALSE)
    print(r2Train, quote=FALSE)
    print("", quote=FALSE)
    print("Training Set R-Squared and Testing Set R-Squared Difference:", quote=FALSE)
    print("", quote=FALSE)
    print(r2Diff, quote=FALSE)
    print("", quote=FALSE)
  }
  sink()
  
  
  close(outFile)
  closeAllConnections()
  
  return(valParams)
  
}
