##################################################################################################################################
#
#  explore_data.R
# 
#  This file generates the descriptive statistic R outputs, the dependent vs independent var plots, the dependent vs. independent
#  dummy variable box plots, the individial var box plots, the independent vs. other independent var plots, the dependent 
#  histogram and normal plot, and the full correlation of all vars to each other.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

explore_data <- function(inData, currentY, currentPlotY, varInfo, makeAllPlots, makeXXPlots, outPath, dataDesc){
  
  # Create output file
  dir.create(file.path(outPath, dataDesc), showWarnings = FALSE)
  explorePath = paste(outPath, dataDesc, "\\", sep="")
  outFileName = paste(explorePath, dataDesc, "_R_Output.txt", sep="")
  outFile = file(outFileName, open="wt")
  
  sumData = summary(inData)
  descData = describe(inData)
  
  allNames = append(currentY, varInfo.xVarNames)
  allPlotNames = append(currentPlotY, varInfo.xVarPlotNames)

    
  # Print descriptive statistics
  sink(file=outFile, append=TRUE)
  print("Data Set Descriptive Statistics Summary:", quote=FALSE)
  print("", quote=FALSE)
  print(sumData, quote=FALSE)
  print("", quote=FALSE)
  print("Data Set Descriptive Statistics Describe Values:", quote=FALSE)
  print("", quote=FALSE)
  print(descData, quote=FALSE)
  print("", quote=FALSE)
  sink()
  
  if(makeAllPlots == TRUE){
    # Plot Y Histogram
    fileName = paste(explorePath, dataDesc, "_", currentY, "Hist.jpeg", sep="")
    plotTitle = paste(currentPlotY, "Histogram")
    jpeg(file=fileName)
    yData = c(t(inData[currentY]))
    hist(yData, prob=TRUE, main=plotTitle, xlab=currentPlotY)
    xFit = seq(min(yData), max(yData), length=length(yData))
    yFit = dnorm(xFit, mean=mean(yData), sd=sd(yData))
    lines(xFit, yFit, col="red", lwd=2)
    dev.off()
    
    # Plot Y Normal Plot
    fileName = paste(explorePath, dataDesc, "_", currentY, "NormPlot.jpeg", sep="")
    plotTitle = paste(currentPlotY, "Normal Plot")
    jpeg(file=fileName)
    qqnorm(yData, main=plotTitle)
    qqline(yData)
    dev.off()
    
    # Boxplot Of All Variables
    numVars = length(allNames)
    dir.create(file.path(explorePath, "Box Plots"), showWarnings = FALSE)
    newPath = paste(explorePath, "Box Plots\\", sep="")
    for (i in 1:numVars){
      var = allNames[i]
      fileName = paste(newPath, dataDesc, "_", allNames[i], "_BoxPlot.jpeg", sep="")
      plotTitle = paste(allPlotNames[i], " Box Plot")
      jpeg(file=fileName)
      plotData = c(t(inData[var]))
      boxplot(plotData, main=plotTitle)
      dev.off()
    }
    
    # Boxplot Of Dummy Variables vs Y Variable
    numXVars = length(varInfo.xVarNames)
    dir.create(file.path(newPath, currentPlotY), showWarnings = FALSE)
    newBoxPath = paste(newPath, currentPlotY, "\\", sep="")
    for (i in 1:numXVars){
      
      var = varInfo.xVarNames[i]
      
      uniqXVar = unique(inData[,var])
      
      if (length(uniqXVar) <= 2){
        fileName = paste(newBoxPath, dataDesc, "_", currentPlotY, "_", varInfo.xVarNames[i], "_BoxPlot.jpeg", sep="")
        plotTitle = paste(currentY, " Versus ", varInfo.xVarNames[i], " Box Plot")
        jpeg(file=fileName)
        boxplot(inData[,currentY]~inData[,var], main=plotTitle)
        dev.off()
        
      }
    }
    
    fileName = paste(newPath, dataDesc, "_AllXBoxPlot.jpeg", sep="")
    plotTitle = paste("All X Variable Box Plot")
    jpeg(file=fileName)
    boxplot(inData[varInfo.xVarNames], main=plotTitle)
    dev.off()
    
    fileName = paste(newPath, dataDesc, "_AllVarBoxPlot.jpeg", sep="")
    plotTitle = paste("All Variable Box Plot")
    jpeg(file=fileName)
    boxplot(inData[allNames], main=plotTitle)
    dev.off()
    
    # Create scatterplots of all x values versus y value
    numXVars = length(varInfo.xVarNames)
    newFolder = paste(currentY, " Scatter Plots", sep="")
    dir.create(file.path(explorePath, newFolder), showWarnings = FALSE)
    newPath = paste(explorePath, newFolder, "\\", sep="")
    for (i in 1:numXVars){
      var = varInfo.xVarNames[i]
      fileName = paste(newPath, dataDesc, "_", var, "Vs", currentY, ".jpeg", sep="")
      plotTitle = paste(currentPlotY, " Versus ", varInfo.xVarPlotNames[i])
      plotData = data.frame(inData[var], inData[currentY])
      jpeg(file=fileName)
      plot(plotData, main=plotTitle,
           xlab=varInfo.xVarPlotNames[i], ylab=currentPlotY)
      dev.off()
    }
    
    if(makeXXPlots == TRUE){
      # Create scatterplots of all x values versus x value
      numXVars = length(varInfo.xVarNames)
      otherVars = varInfo.xVarNames
      for (i in 1:numXVars){
        
        var = varInfo.xVarNames[i]
        otherVars = setdiff(otherVars, var)
        numOtherVars = length(otherVars)
        
        if(numOtherVars > 0){
          newFolder = paste(var, " Scatter Plots", sep="")
          dir.create(file.path(explorePath, newFolder), showWarnings = FALSE)
          newPath = paste(explorePath, newFolder, "\\", sep="")
          
          
          for (j in 1:numOtherVars){
            otherVar = otherVars[j]
            otherNdx = match(otherVar, varInfo.xVarNames)
            fileName = paste(newPath, dataDesc, "_", var, "Vs", otherVar, ".jpeg", sep="")
            plotTitle = paste(varInfo.xVarPlotNames[i], " Versus ", varInfo.xVarPlotNames[otherNdx])
            plotData = data.frame(inData[otherVar], inData[var])
            jpeg(file=fileName)
            plot(plotData, main=plotTitle,
                 xlab=varInfo.xVarPlotNames[otherNdx], ylab=varInfo.xVarPlotNames[i])
            dev.off()
            
          }
        }
      }
    }
  }
  
    
  # Create Correlation Values for all data
  numAllVars = length(allNames)
  sink(file=outFile, append=TRUE)
  print("", quote=FALSE)

  for(i in 1:numAllVars){
    currentCor = cor(inData[allNames[i]], inData[allNames])
    
    assign("last.warning", NULL, envir = baseenv())
    
    print(paste("Correlation of ", allNames[i], " and rest of variables:", sep=""), quote=FALSE)
    print("", quote=FALSE)
    print(currentCor, quote=FALSE)
    print("", quote=FALSE)
    print("", quote=FALSE)

    assign("last.warning", NULL, envir = baseenv())
    
  }  

  sink()
  
  close(outFile)
  closeAllConnections()
  
  return(explorePath)
}
