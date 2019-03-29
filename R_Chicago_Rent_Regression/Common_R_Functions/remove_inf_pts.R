##################################################################################################################################
#
#  remove_inf_pts.R
# 
#  This file prints out the rows of the data set given by the infPtsNdx passed in, then removes the rows from the data set.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

remove_inf_pts <- function(inData, infPtsNdx, outPath, ptDesc){
  
  # Create output file
  outFileName = paste(basePath, "_Remove_", ptDesc, "_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  
  numOutPresent = length(infPtsNdx)
  numRowsOut = nrow(inData) - length(infPtsNdx)
  perRmv = (numOutPresent/numRowsOut)*100
  
  # Print outliers found
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)
  print(paste("Number of influential points found: ", numOutPresent, sep=""), quote=FALSE)
  print("", quote=FALSE)
  print(paste("Number of remaining rows in the dataset: ", numRowsOut, sep=""), quote=FALSE)
  print("", quote=FALSE)
  print(paste("Percentage of dataset removed: ", format(round(perRmv, 2), nsmall = 2), " %", sep=""), quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Rows removed: ", quote=FALSE)
  print("", quote=FALSE)
  print(inData[infPtsNdx,], quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  close(outFile)
  closeAllConnections()
  
  outData = inData[-infPtsNdx,]
  
  return(outData)
  
}
