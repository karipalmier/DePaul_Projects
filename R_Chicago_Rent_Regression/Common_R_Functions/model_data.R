##################################################################################################################################
#
#  model_data.R
# 
#  This file calls a function to generate all of the exploratory plots and R outputs, creates the model using the data passed in,
#  then calls functions that analyze the model, perform cross validation, perform training/testing validation, and perform 
#  predictions.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

model_data <- function(inData, testData, modelFlags, percTestPts, percCrossValPts, outlierLevel, varInfo, outPath, dataDesc){

  numXVars = length(varInfo.xVarNames)
  for(j in 1:numXVars){
    var = varInfo.xVarNames[j]
    
    if(j == 1){
      xformStr = var
    }else{
      xformStr = paste(xformStr, "+", var, sep="")
    }
  }
  
  yOutName = paste(toupper(substr(varInfo.yVarName, 1, 1)), substr(varInfo.yVarName, 2, nchar(varInfo.yVarName)), sep="")
  
  dir.create(file.path(outPath, yOutName), showWarnings = FALSE)
  basePath = paste(outPath, yOutName, "\\", sep="")
    
  ################################################## Exploratory Step #########################################################

  if(modelFlags.performExp == TRUE){
    
    # Perform Exploratory Data Analysis after outlier removal
    exploreStr = paste("Explore_", dataDesc, "_", yOutName, sep="")
    
    if(i == 1){
      explorePath = explore_data(inData, varInfo.yVarName, varInfo.yVarPlotName, varInfo, modelFlags.makeExpPlots, 
                              modelFlags.makeExpXXPlots, basePath, exploreStr)
    }else {
      explorePath = explore_data(inData, varInfo.yVarName, varInfo.yVarPlotName, varInfo, modelFlags.makeExpPlots, 
                              FALSE, basePath, exploreStr)
    }
    
    if(numXVars <= 20){
      if(modelFlags.makeExpPlots == TRUE){
        
        # Plot Matrix of Scatterplots
        exploreBasePath = paste(basePath, exploreStr, "\\", sep="")
        fileName = paste(exploreBasePath, exploreStr, "_ScatterPlotMatrix.jpeg", sep="")
        plotTitle = "Scatterplot Matrix For All Variables"
        jpeg(file=fileName)
        
        formStr = paste("~", varInfo.yVarName, "+", xformStr, sep="")
        pairForm = as.formula(formStr)
        pairs(pairForm, data=inData, main=plotTitle)
        dev.off()
      }
    }
    
  }
  
  
  ############################################### Data Modelling Step ##########################################################
  
  if(modelFlags.performModel == TRUE){
    modelStr = paste("Model_", dataDesc, "_", yOutName, sep="")
    
    dir.create(file.path(basePath, modelStr), showWarnings = FALSE)
    modelPath = paste(basePath, modelStr, "\\", sep="")
    
    # Create model with all x variables and data and perform model analysis
    if(modelFlags.quadXModel == TRUE){
      formStr = paste(varInfo.yVarName, "~(", xformStr, ")^2", sep="")
    }else{
      formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
    }
    
    modelForm = as.formula(formStr)
    
    inModel = lm(modelForm, data=inData)
    
    fitSummary = summary(inModel)
    
    modelOutParams = vector("list", 11)
    modelOutParams[[1]] = analyze_model(inModel, inData, modelForm, varInfo, modelPath, modelStr)
    
    stdResiduals = rstandard(inModel)
    outlierVal = abs(stdResiduals) > outlierLevel
    outlierNdx = which(outlierVal %in% TRUE)
    modelOutParams[[2]] = outlierNdx
    
    fStatInfo = fitSummary$fstatistic
    modelOutParams[[3]] = fitSummary$r.squared
    modelOutParams[[4]] = fitSummary$adj.r.squared
    modelOutParams[[5]] = fStatInfo[1]
    modelOutParams[[6]] = pf(fStatInfo[1],fStatInfo[2],fStatInfo[3],lower.tail=F)
    modelOutParams[[7]] = fitSummary$coefficients
    modelOutParams[[8]] = vif(inModel)
    

    baseFormStr = paste(varInfo.yVarName, "~1", sep="")
    baseForm = as.formula(baseFormStr)
    
    baseModel = lm(baseForm, data=inData)
    modelOutParams[[9]] = select_model(inModel, inData, varInfo.yVarName, baseModel, varInfo, modelPath, modelStr)
    
    modelOutParams[[10]] = crossvalidate_data(inModel, inData, percCrossValPts, varInfo.yVarName, varInfo, modelPath, modelStr)
    
    modelOutParams[[11]] = validate_data(inModel, inData, testData, varInfo.yVarName, percTestPts, modelFlags.validateTest, 
                             varInfo, modelPath, modelStr)
    
  }
    
  return(modelOutParams)
}