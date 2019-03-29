##################################################################################################################################
#
#  get_outliers.R
# 
#  This file finds the rows of the data set passed in that contain the outlierCond passed in the columns specified by the 
#  checkVars var.  The rows that match these conditions are then removed from the data set.  The rows removed are printed out.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

get_outliers <- function(inData, outlierValue, outlierCond, checkVars, outPath, dataDesc){
  
  # Create output file
  upOutlierCond = toupper(outlierCond)
  outlierValStr = toString(outlierValue)
  outlierValPStr = sub("\\.", "p", outlierValStr)

  dir.create(file.path(outPath, dataDesc), showWarnings = FALSE)
  basePath = paste(outPath, dataDesc, "\\", sep="")
  outFileName = paste(basePath, dataDesc, "_", upOutlierCond, "_", outlierValPStr, "_Outliers_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  

  # Create Box Cox formula string
  numVars = length(checkVars)
  outData = inData
  allMatchNdx = c()
  for (i in 1:numVars){
    
    var = checkVars[i]
    
    if(upOutlierCond == "GTE"){
      matchVals = inData[, var] >= outlierValue
    }else if(upOutlierCond == "GT"){
      matchVals = inData[, var] > outlierValue
    }else if(upOutlierCond == "LTE"){
      matchVals = inData[, var] <= outlierValue
    }else if(upOutlierCond == "LT"){
      matchVals = inData[, var] < outlierValue
    }else if(upOutlierCond == "EQ"){
      matchVals = inData[, var] == outlierValue
    }
    
    matchNdx = which(matchVals %in% TRUE)
    
    if(length(inData[matchNdx,var]) > 0){
      allMatchNdx = append(allMatchNdx, matchNdx)
      allMatchNdx = unique(allMatchNdx)
    }
  }
  
  numOutPresent = length(allMatchNdx)
  numRowsOut = nrow(inData) - length(allMatchNdx)
  perRmv = (numOutPresent/numRowsOut)*100
  
  if(upOutlierCond == "GTE"){
    condStr = "greater than or equal to"
  }else if(upOutlierCond == "GT"){
    condStr = "greater than"
  }else if(upOutlierCond == "LTE"){
    condStr = "less than or equal to"
  }else if(upOutlierCond == "LT"){
    condStr = "less than"
  }else if(upOutlierCond == "EQ"){
    condStr = "equal to"
  }

  # Print outliers found
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print(paste("Number of outliers found ", condStr, " value = ", outlierValStr, ": ", numOutPresent, sep=""), quote=FALSE)
  print("", quote=FALSE)
  print(paste("Number of remaining rows in the dataset: ", numRowsOut, sep=""), quote=FALSE)
  print("", quote=FALSE)
  print(paste("Percentage of dataset removed: ", format(round(perRmv, 2), nsmall = 2), " %", sep=""), quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Rows removed: ", quote=FALSE)
  print("", quote=FALSE)
  print(inData[allMatchNdx,], quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  outData = outData[-allMatchNdx,]

  close(outFile)
  closeAllConnections()
  
  return(outData)
  
}
