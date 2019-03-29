

ModelTimeData <- function(
  timeData = NULL,
  ts_orders = NULL,
  periodicity = NULL,
  season_orders = NULL,
  use_regressors = NULL,
  reg_data = NULL,
  filePath = NULL,
  data_desc = NULL
) {
  
  model_order = sum(ts_orders)
  order_str = paste(ts_orders[1], '_', ts_orders[2], '_', ts_orders[3], sep = '')
  season_str = paste(season_orders[1], '_', season_orders[2], '_', season_orders[3], sep = '')
  
  if ((periodicity > 0) && (use_regressors == TRUE)){
    ts_model = Arima(timeData, order = ts_orders, seasonal = list(order = season_orders, period = periodicity), xreg = reg_data, method = "ML")
    model_desc = paste(data_desc, '_ARIMA_', order_str, '_Season_', season_str, '_W_X_Regs', sep = '')
  }else if (periodicity > 0){
    ts_model = Arima(timeData, order = ts_orders, seasonal = list(order = season_orders, period = periodicity), method = "ML")
    model_desc = paste(data_desc, '_ARIMA_', order_str, '_Season_', season_str, sep = '')
  }else if (use_regressors == TRUE){
    ts_model = Arima(timeData, order = ts_orders, xreg = reg_data, method = "ML")
    model_desc = paste(data_desc, '_ARIMA_', order_str, '_X_Regs', sep = '')
  }else {
    ts_model = Arima(timeData, order = ts_orders, method = "ML")
    model_desc = paste(data_desc, '_ARIMA_', order_str, sep = '')
  }
  
  dir.create(file.path(filePath, model_desc), showWarnings = FALSE)
  model_path = paste(filePath, model_desc, "\\", sep="")
  
  num_coefs = length(ts_model$coef)
  model_coefs = ts_model$coef
  coef_names = names(model_coefs)
  
  ma_coefs = c()
  ar_coefs = c()
  ma_ndx = 1
  ar_ndx = 1
  
  for (i in 1:num_coefs){
    if (grepl("ma", coef_names[i])){
      ma_coefs[ma_ndx] = model_coefs[coef_names[i]]
      ma_ndx = ma_ndx + 1
    }else if (grepl("ar", coef_names[i])){
      ar_coefs[ar_ndx] = model_coefs[coef_names[i]]
      ar_ndx = ar_ndx + 1
    }
  }
  
  if (!is.null(ar_coefs)){
    ar_roots = polyroot(c(1, ar_coefs))
  }
  
  if (!is.null(ma_coefs)){
    ma_roots = polyroot(c(1, ma_coefs))
  }
  
  out_file_name = paste(model_path, 'Model_Analysis.txt', sep = '')
  outFile = file(out_file_name, open="wt")
  
  sink(file = outFile, append = TRUE)
  
  print ('*** Model Order ***')
  print(model_order)
  print("", quote=FALSE)
  
  # Model summary and coef test
  print('*** Model Summary ***')
  print(summary(ts_model))
  print("", quote=FALSE)
  print("", quote=FALSE)
  
  print('*** Model Coefficient Tests ***')
  print(coeftest(ts_model))
  print("", quote=FALSE)
  
  print('*** Model Polynomial Roots (> 1 = Stat) ***')
  if (!is.null(ar_coefs)){
    print('AR Roots:')
    print(ar_roots)
    print("", quote=FALSE)
  }
  
  if (!is.null(ma_coefs)){
    print('MA Roots:')
    print(ma_roots)
    print("", quote=FALSE)
  }
  print("", quote=FALSE)
  
  # Perform Jarque-Bera normality test
  print('*** Residual Jarque Bera Normality Test ***')
  print(normalTest(ts_model$resid, method=c("jb")))
  print("", quote=FALSE)
  
  # Ljung Box of residuals
  print("*** Residual Ljung Box Test With 5 Lags")
  print(Box.test(ts_model$resid, lag = 5, type = 'Ljung', fitdf = model_order))
  print("", quote=FALSE)
  
  print("*** Residual Ljung Box Test With 10 Lags")
  print(Box.test(ts_model$resid, lag = 10, type = 'Ljung', fitdf = model_order))
  print("", quote=FALSE)
  
  print("*** Residual Ljung Box Test With 25 Lags")
  print(Box.test(ts_model$resid, lag = 25, type = 'Ljung', fitdf = model_order))
  print("", quote=FALSE)
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller zero mean no trend
  print('*** Augmented Dickey Fuller Test - No Intercept (Zero Mean), No Time Trend ***')
  print(adfTest(ts_model$resid, lag = 5, type = c('nc'))) #model_order
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller constant mean no trend
  print('*** Augmented Dickey Fuller Test - Constant Intercept (Constant Mean), No Time Trend ***')
  print(adfTest(ts_model$resid, lag = 5, type = c('c')))
  print("", quote=FALSE)
  
  #Augmented Dickey Fuller constant mean no trend
  print('*** Augmented Dickey Fuller Test - Constant Intercept (Constant Mean), Time Trend ***')
  print(adfTest(ts_model$resid, lag = 5, type = c('ct')))
  print("", quote=FALSE)
  
  print('*** 80% Training, 20% Validation Backtesting ***')
  if (use_regressors == TRUE){
    print(backtest(ts_model, timeData, h = 1, orig = length(timeData)*0.8, xre = reg_data))
  }else{
    print(backtest(ts_model, timeData, h = 1, orig = length(timeData)*0.8))
  }
  
  sink()
  
  close(outFile)
  closeAllConnections()

  ############################################ Scatterplots #############################################
  
  tmp_scatter = paste(model_path, 'Residual_Scatter.jpg', sep = '')
  jpeg(file = tmp_scatter)
  x_title = 'Date'
  y_title = 'Residuals'
  plot_title = 'Residual Vs. Time'
  plot(ts_model$resid, xlab = x_title, ylab = y_title, main = plot_title)
  dev.off()
  
  ############################################Normal Plots ##############################################
  
  tmp_qq = paste(model_path, 'Residual_QQ.jpg', sep = '')
  jpeg(file = tmp_qq)
  plot_title = 'Residual Normal Plot'
  qqnorm(ts_model$resid, main = plot_title)
  qqline(ts_model$resid)
  dev.off()
  
  ############################################ ACF Plots ################################################
  
  tmp_acf = paste(model_path, 'Residual_ACF.jpg', sep = '')
  jpeg(file = tmp_acf)
  plot_title = 'Residual ACF'
  acf(ts_model$resid, plot = TRUE, na.action = na.exclude, main = plot_title)
  dev.off()

  ########################################## Backtest Plots #############################################
  
  if (use_regressors == TRUE){
    bt_data = backtest(ts_model, timeData, h = 1, orig = length(timeData)*0.8, xre = reg_data)
  }else{
    bt_data = backtest(ts_model, timeData, h = 1, orig = length(timeData)*0.8)
  }
  x = seq(1, length(bt_data$obs), by=1)

  tmp_bt_scatter = paste(model_path, 'Backtest_Forecast.jpg', sep = '')
  jpeg(file = tmp_bt_scatter)
  x_title = 'Index of Validation Points'
  y_title = 'Validation Values (Obs = Black, Pred = Red)'
  plot_title = 'Backtest Observed and Forecasted Validation Points'
  plot(x, bt_data$obs, type='l', col='black', xlab = x_title, ylab = y_title, main = plot_title)
  lines(x, bt_data$forecast, type='l', col='red')
  
  dev.off()
  
  tmp_bt2_scatter = paste(model_path, 'Backtest_Error.jpg', sep = '')
  jpeg(file = tmp_bt2_scatter)
  x_title = 'Index of Validation Points'
  y_title = 'Back Testing Error'
  plot_title = 'Back Testing Error (Observed - Forecasted)'
  plot(x, bt_data$error, type='l', col='black', xlab = x_title, ylab = y_title, main = plot_title)

  dev.off()
  
  
  return(ts_model)
  
}
