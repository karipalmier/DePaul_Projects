

CrossCorrTimeData <- function(
  allSymbols = NULL,
  plotPath = NULL,
  dataPath = NULL,
  dataSuffix = NULL
) {
  
  ########################################## Cross Correlation Function Plotting #########################################
  
  numVars = length(allSymbols)
  for (i in 1:numVars){
    
    var = allSymbols[i]
    otherVars = setdiff(allSymbols, var)
    numOtherVars = length(otherVars)
    
    var_path = paste(dataPath, var, dataSuffix, sep='')
    tmp_var = read.table(var_path, header = T, sep = ',')
    var_data = tmp_var[,2]
    
    var_dates = tryCatch(as.Date(as.character(tmp_var[,3])), error=function(e) as.Date(as.yearmon(tmp_var[,3])))
    
    var_ts = zoo(var_data, var_dates)
    
    if(numOtherVars > 0){
      dir.create(file.path(plotPath, "CCF"), showWarnings = FALSE)
      newPath = paste(plotPath, "CCF\\", sep="")
      
      for (j in 1:numOtherVars){
        otherVar = otherVars[j]
        otherNdx = match(otherVar, allSymbols)
        fileName = paste(newPath, "CCF_", var, "_Vs_", otherVar, ".jpeg", sep="")
        
        other_var_path = paste(dataPath, otherVar, dataSuffix, sep='')
        tmp_other_var = read.table(other_var_path, header = T, sep = ',')
        other_var_data = tmp_other_var[,2]
        
        other_var_dates = tryCatch(as.Date(as.character(tmp_other_var[,3])), error=function(e) other_var_dates = as.Date(as.yearmon(tmp_other_var[,3])))
        
        other_var_ts = zoo(other_var_data, other_var_dates)

        plotTitle = paste("CCF", allSymbols[i], " Versus ", allSymbols[otherNdx])

        jpeg(file=fileName)
        ccf(var_ts, other_var_ts, plot = TRUE, na.action = na.pass, main = plotTitle)
        dev.off()
        
      }
    }
  }
}
  