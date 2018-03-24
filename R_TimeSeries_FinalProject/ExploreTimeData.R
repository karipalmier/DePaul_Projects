

ExploreTimeData <- function(
  timeData = NULL,
  dataSymbol = NULL,
  filePath = NULL,
  plotDesc = NULL,
  numLags = 25
) {
 
  ############################################ Histograms ###############################################
  
  dir.create(file.path(filePath, "Histograms"), showWarnings = FALSE)
  hist_path = paste(filePath, "Histograms\\", sep="")

  tmp_hist = paste(hist_path, dataSymbol, '_Hist.jpg', sep = '')
  jpeg(file = tmp_hist)
  x_title = paste(dataSymbol, plotDesc)
  plot_title = paste(dataSymbol, plotDesc, 'Histogram')
  hist(timeData, xlab = x_title, prob = TRUE, main = plot_title)
  # add approximating normal density curve
  xfit = seq(min(timeData), max(timeData), length=40)
  yfit = dnorm(xfit, mean = mean(timeData), sd = sd(timeData))
  lines(xfit, yfit, col = "blue", lwd = 2) 
  dev.off()
  
  ############################################ Scatterplots #############################################
  
  dir.create(file.path(filePath, "Scatterplots"), showWarnings = FALSE)
  scatter_path = paste(filePath, "Scatterplots\\", sep="")
  
  tmp_scatter = paste(scatter_path, dataSymbol, '_Scatter.jpg', sep = '')
  jpeg(file = tmp_scatter)
  x_title = 'Date'
  y_title = paste(dataSymbol, plotDesc)
  plot_title = paste(dataSymbol, plotDesc, 'Vs. Time')
  plot(timeData, xlab = x_title, ylab = y_title, main = plot_title)
  dev.off()
  
  ############################################Normal Plots ##############################################
  
  dir.create(file.path(filePath, "QQ"), showWarnings = FALSE)
  qq_path = paste(filePath, "QQ\\", sep="")
  
  tmp_qq = paste(qq_path, dataSymbol, '_QQ.jpg', sep = '')
  jpeg(file = tmp_qq)
  plot_title = paste(dataSymbol, plotDesc, 'Return')
  qqnorm(timeData, main = plot_title)
  qqline(timeData)
  dev.off()
  
  ############################################ ACF Plots ################################################
  
  dir.create(file.path(filePath, "ACF"), showWarnings = FALSE)
  acf_path = paste(filePath, "ACF\\", sep="")
  
  tmp_acf = paste(acf_path, dataSymbol, '_ACF.jpg', sep = '')
  jpeg(file = tmp_acf)
  plot_title = paste(dataSymbol, plotDesc)
  acf(timeData, plot = TRUE, na.action = na.exclude, main = plot_title)
  dev.off()
  
  ############################################ PACF Plots ###############################################
  
  dir.create(file.path(filePath, "PACF"), showWarnings = FALSE)
  pacf_path = paste(filePath, "PACF\\", sep="")
  
  tmp_pacf = paste(pacf_path, dataSymbol, '_PACF.jpg', sep = '')
  jpeg(file = tmp_pacf)
  plot_title = paste(dataSymbol, plotDesc)
  
  pacf(timeData, plot = TRUE, na.action = na.exclude, main = plot_title)
  
  dev.off()
  
  ############################################ Statistics ###############################################
  
  out_file_name = paste(filePath, dataSymbol, '_Statistics.txt', sep = '')
  outFile = file(out_file_name, open="wt")
  
  sink(file = outFile, append = TRUE)
  
  # Basic Statistics
  print('*** Basic Statistics ***')
  print(basicStats(timeData))
  print("", quote=FALSE)
  
  # Perform Jarque-Bera normality test
  print('*** Jarque Bera Normality Test ***')
  print(normalTest(timeData, method=c("jb")))
  print("", quote=FALSE)
  
  # Fat-tail test
  print('*** Kurtosis (Tail) Test ***')
  k_stat = kurtosis(timeData) / sqrt(24/length(timeData))
  print("Kurtosis Test Statistic:")
  print(k_stat)
  print("P-value:")
  tmp_p = 2*(1 - pnorm(abs(k_stat)))
  print(tmp_p)
  print("", quote=FALSE)
  
  #Skewness test
  print('*** Skewness Test ***')
  skew_test = skewness(timeData) / sqrt(6/length(timeData))
  print("Skewness Test Statistic:")
  print(skew_test)
  print("P-value:")
  tmp_p = 2* (1 - pnorm(abs(skew_test)))
  print(tmp_p)
  print("", quote=FALSE)
  
  #Ljung Box test with 25 lags
  print('*** Ljung Box Serial Correlation Test ***')
  print(Box.test(timeData, lag = numLags, type = 'Ljung'))
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller zero mean no trend
  print('*** Augmented Dickey Fuller Test - No Intercept (Zero Mean), No Time Trend ***')
  print(adfTest(timeData, lag = numLags, type = c('nc')))
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller constant mean no trend
  print('*** Augmented Dickey Fuller Test - Constant Intercept (Constant Mean), No Time Trend ***')
  print(adfTest(timeData, lag = numLags, type = c('c')))
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller constant mean no trend
  print('*** Augmented Dickey Fuller Test - Constant Intercept (Constant Mean), Time Trend ***')
  print(adfTest(timeData, lag = numLags, type = c('ct')))
  print("", quote=FALSE)
  
  
  sink()
  
  close(outFile)
  closeAllConnections()
  
}

