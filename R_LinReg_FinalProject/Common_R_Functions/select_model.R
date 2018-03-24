##################################################################################################################################
#
#  select_model.R
# 
#  This file performs model selection on the model and input data passed in.  It generates the R output for the R-squared, Cp,
#  backward, stepwise, and forward model selection methods.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

select_model <- function(inModel, inData, currentY, baseModel, varInfo, basePath, modelDesc){
  
  # Create output file
  outFileName = paste(basePath, modelDesc, "_Selection_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  

  # Create matrix of X variables and y variable to prepare for model selection
  xData = inData[varInfo.xVarNames]
  yData = c(t(inData[currentY]))
  
  # Perform CP model selection
  allVarNames = cbind(currentY, varInfo.xVarNames)
  numAllVars = length(allVarNames)
  
  if(numAllVars <= 31){
    leapCp = leaps(x=xData, y=yData, names=varInfo.xVarNames, method="Cp")
    cpMatrix = cbind(leapCp$size, leapCp$which, leapCp$Cp)
    
    # Display results in increasing order of Cp
    cpSubset = head(cpMatrix[order(cpMatrix[, dim(cpMatrix)[2]]),])
    
    # Print cp model selection values to output file
    sink(file=outFile, append=TRUE)
    print("", quote=FALSE)
    print("Output of Cp Model Selection:", quote=FALSE)
    print("", quote=FALSE)
    print(leapCp, quote=FALSE)
    print("", quote=FALSE)
    print("Cp Model Subsets of Interest:", quote=FALSE)
    print(cpSubset, quote=FALSE)
    print("", quote=FALSE)
    sink()
    
    # Perform adj-R2 model selection
    leapAdjR2 = leaps(x=xData, y=yData, names=varInfo.xVarNames, method="adjr2")
    adjR2Matrix = cbind(leapAdjR2$size, leapAdjR2$which, leapAdjR2$adjr2)
    
    # Display results in increasing order of Cp
    adjR2Subset = head(adjR2Matrix[order(adjR2Matrix[, dim(adjR2Matrix)[2]], decreasing=T),])
    
    # Print adjusted r2 model selection to output file
    sink(file=outFile, append=TRUE)
    print("", quote=FALSE)
    print("Output of Adusted R-Squared Model Selection:", quote=FALSE)
    print("", quote=FALSE)
    print(leapAdjR2, quote=FALSE)
    print("", quote=FALSE)
    print("Adusted R-Squared Model Subsets of Interest:", quote=FALSE)
    print(adjR2Subset, quote=FALSE)
    print("", quote=FALSE)
    sink()
  }else{
    sink(file=outFile, append=TRUE)
    print("", quote=FALSE)
    print("Too many variables for Cp and Adjusted R-Squared Model Selection", quote=FALSE)
    print("", quote=FALSE)
    sink()
    
  }
  
  # Perform Backward model selection
  backSel = step(inModel, direction="backward", trace=FALSE)
  
  # Print backward selection values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Output of Backward Model Selection:", quote=FALSE)
  print("", quote=FALSE)
  print(backSel, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  # Create a vector of names for forward sel
  tempVar = backSel$coefficients
  tempDf = data.frame(tempVar)
  backNames = rownames(tempDf)

  # Perform Stepwise model selection and print to output file
  stepSel = step(baseModel, scope=list(upper=inModel, lower=~1), direction="both", trace=FALSE)
  
  # Print stepwise selection values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Output of Stepwise Model Selection:", quote=FALSE)
  print("", quote=FALSE)
  print(stepSel, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  # Create a vector of names for forward sel
  tempVar = stepSel$coefficients
  tempDf = data.frame(tempVar)
  stepNames = rownames(tempDf)
  
  # Perform Forward model selection
  forwSel = step(baseModel, scope=list(upper=inModel, lower=~1), direction="forward", trace=FALSE)
  
  # Print forward model selection values to output file
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print("Output of Forward Model Selection:", quote=FALSE)
  print("", quote=FALSE)
  print(forwSel, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  # Create a vector of names for forward sel
  tempVar = forwSel$coefficients
  tempDf = data.frame(tempVar)
  forwNames = rownames(tempDf)
  
  close(outFile)
  closeAllConnections()
  
  selNames = vector("list", 3)
  selNames[[1]] = backNames
  selNames[[2]] = stepNames
  selNames[[3]] = forwNames
  
  return(selNames)
  
}
