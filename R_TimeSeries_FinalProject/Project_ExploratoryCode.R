library(zoo)
library(tseries)
library(fBasics)
library(fUnitRoots)

# Get the path of the current based on if RStudio is run or if run from command line
cmdArgs <- commandArgs(trailingOnly = FALSE)

# If run from RStudio, use the rstudio api to get the path of the current R script
mainPath = dirname(rstudioapi::getSourceEditorContext()$path)
mainPath = gsub("/", "\\\\", mainPath)
mainPath = paste(mainPath, '\\', sep="")

source(paste(mainPath, "ExploreTimeData.R", sep=""))
source(paste(mainPath, "CrossCorrTimeData.R", sep=""))

dir.create(file.path(mainPath, "R_Output"), showWarnings = FALSE)
mainAnlysPath = paste(mainPath, "R_Output\\", sep="")

dir.create(file.path(mainAnlysPath, "Exploratory"), showWarnings = FALSE)
mainAnlysPath = paste(mainAnlysPath, "Exploratory\\", sep="")

# Read in NYSE data
nyse_all = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\Dataset\\prices-split-adjusted.csv', header = T, sep = ',')
nyse_all$date = as.character(nyse_all$date)
nyse_all$symbol = as.character(nyse_all$symbol)

# Stock symbols of interest
stock_syms = c('AAPL', 'GOOGL', 'GOOG', 'CSCO', 'HPQ', 'INTC', 'MSFT', 'NVDA', 'ORCL', 'RHT', 'TXN', 'WDC', 'XRX', 'YHOO')

num_stocks = length(stock_syms)
for (i in 1:num_stocks){
  
  tmp_symbol = stock_syms[i]
  tmp_ndx = nyse_all$symbol == tmp_symbol
  tmp_data = nyse_all[tmp_ndx,]
  tmp_dates = as.Date(tmp_data$date)

  num_rows = nrow(tmp_data)
  tmp_short_dates = as.Date(tmp_data$date[2:num_rows])
  
  
  ########################################################################################################################
  ###################################################### Daily Data ######################################################
  ########################################################################################################################
  
  dir.create(file.path(mainAnlysPath, "Daily"), showWarnings = FALSE)
  anlysPath = paste(mainAnlysPath, "Daily\\", sep="")

  ########################################## Time Series Exploratory Data ################################################
  
  print(paste(tmp_symbol, 'TS Daily Calculations'))
  
  tmp_data_ts = zoo(tmp_data$close, tmp_dates)
  
  data_desc = "Time Series"
  dir.create(file.path(anlysPath, "TS"), showWarnings = FALSE)
  ts_path = paste(anlysPath, "TS\\", sep="")
  
  num_lags = floor(length(tmp_data_ts)/3)
  
  ExploreTimeData(timeData = tmp_data_ts, dataSymbol = tmp_symbol, filePath = ts_path, plotDesc = data_desc, numLags = 5)

  dir.create(file.path(ts_path, "Data"), showWarnings = FALSE)
  ts_csv_path = paste(ts_path, "Data\\", sep="")
  
  ts_df = data.frame(tmp_data_ts)
  ts_df$date = rownames(ts_df)
  rownames(ts_df) <- 1:nrow(ts_df)
  
  full_ts_csv_path = paste(ts_csv_path, tmp_symbol, '_TS_Data.csv', sep = '')
  write.csv(ts_df, full_ts_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = ts_path, dataPath = ts_csv_path, dataSuffix = '_TS_Data.csv')
  }
  
  
  ####################################### Log Time Series Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Log TS Daily Calculations'))
  
  log_data = log(tmp_data$close)
  log_data_ts = zoo(log_data, tmp_dates)
  
  data_desc = "Log Time Series"
  dir.create(file.path(anlysPath, "Log_TS"), showWarnings = FALSE)
  log_path = paste(anlysPath, "Log_TS\\", sep="")
  
  num_lags = floor(length(log_data_ts)/3)
  
  ExploreTimeData(timeData = log_data_ts, dataSymbol = tmp_symbol, filePath = log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(log_path, "Data"), showWarnings = FALSE)
  log_csv_path = paste(log_path, "Data\\", sep="")
  
  log_df = data.frame(log_data_ts)
  log_df$date = rownames(log_df)
  rownames(log_df) <- 1:nrow(log_df)
  
  full_log_csv_path = paste(log_csv_path, tmp_symbol, '_Log_Data.csv', sep = '')
  write.csv(log_df, full_log_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = log_path, dataPath = log_csv_path, dataSuffix = '_Log_Data.csv')
  }
  
  ####################################### Diff Time Series Exploratory Data ##############################################
  
  print(paste(tmp_symbol, 'Diff TS Daily Calculations'))
  
  diff_data = diff(tmp_data$close)
  diff_data_ts = zoo(diff_data, tmp_short_dates)
  
  data_desc = "Diff Time Series"
  dir.create(file.path(anlysPath, "Diff_TS"), showWarnings = FALSE)
  diff_path = paste(anlysPath, "Diff_TS\\", sep="")
  
  num_lags = floor(length(diff_data_ts)/3)
  
  ExploreTimeData(timeData = diff_data_ts, dataSymbol = tmp_symbol, filePath = diff_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_path, "Data"), showWarnings = FALSE)
  diff_csv_path = paste(diff_path, "Data\\", sep="")
  
  diff_df = data.frame(diff_data_ts)
  diff_df$date = rownames(diff_df)
  rownames(diff_df) <- 1:nrow(diff_df)
  
  full_diff_csv_path = paste(diff_csv_path, tmp_symbol, '_Diff_Data.csv', sep = '')
  write.csv(diff_df, full_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_path, dataPath = diff_csv_path, dataSuffix = '_Diff_Data.csv')
  }
  
  ###################################### Log Diff Time Series Exploratory Data ###########################################
  
  print(paste(tmp_symbol, 'Diff Log TS Daily Calculations'))
  
  diff_log_data = diff(log(tmp_data$close))
  diff_log_data_ts = zoo(diff_log_data, tmp_short_dates)
  
  data_desc = "Diff Log Time Series"
  dir.create(file.path(anlysPath, "Diff_Log_TS"), showWarnings = FALSE)
  diff_log_path = paste(anlysPath, "Diff_Log_TS\\", sep="")
  
  num_lags = floor(length(diff_log_data_ts)/3)
  
  ExploreTimeData(timeData = diff_log_data_ts, dataSymbol = tmp_symbol, filePath = diff_log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_log_path, "Data"), showWarnings = FALSE)
  log_diff_csv_path = paste(diff_log_path, "Data\\", sep="")
  
  log_diff_df = data.frame(diff_log_data_ts)
  log_diff_df$date = rownames(log_diff_df)
  rownames(log_diff_df) <- 1:nrow(log_diff_df)
  
  full_log_diff_csv_path = paste(log_diff_csv_path, tmp_symbol, '_LogDiff_Data.csv', sep = '')
  write.csv(log_diff_df, full_log_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_log_path, dataPath = log_diff_csv_path, dataSuffix = '_LogDiff_Data.csv')
  }
  
  ############################################### Return Data Creation ###################################################
  
  tmp_gross_rtn = rep(1, num_rows)
  tmp_simple_rtn = rep(1, num_rows)
  
  for (j in 1:num_rows){
    
    if (j == 1){
      tmp_gross_rtn[j] = NA
      tmp_simple_rtn[j] = NA
    }
    else{
      tmp_gross_rtn[j] = tmp_data$close[j] / tmp_data$close[(j-1)]
      tmp_simple_rtn[j] = (tmp_data$close[j] / tmp_data$close[(j-1)]) - 1
    }
  }
  
  tmp_data$gross_return = tmp_gross_rtn
  tmp_data$simple_return = tmp_simple_rtn
  
  gross_rtn = tmp_gross_rtn[2:num_rows]
  simple_rtn = tmp_simple_rtn[2:num_rows]
  
  tmp_gross_rtn_ts = zoo(gross_rtn, tmp_short_dates)
  tmp_simple_rtn_ts = zoo(simple_rtn, tmp_short_dates)
  
  ########################################## Gross Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Gross Return TS Daily Calculations'))
  
  data_desc = "Gross Return"
  dir.create(file.path(anlysPath, "Gross_Return"), showWarnings = FALSE)
  gross_path = paste(anlysPath, "Gross_Return\\", sep="")
  
  num_lags = floor(length(tmp_gross_rtn_ts)/3)
  
  ExploreTimeData(timeData = tmp_gross_rtn_ts, dataSymbol = tmp_symbol, filePath = gross_path, plotDesc = data_desc, numLags = 5)

  dir.create(file.path(gross_path, "Data"), showWarnings = FALSE)
  gross_csv_path = paste(gross_path, "Data\\", sep="")
  
  gross_df = data.frame(tmp_gross_rtn_ts)
  gross_df$date = rownames(gross_df)
  rownames(gross_df) <- 1:nrow(gross_df)
  
  full_gross_csv_path = paste(gross_csv_path, tmp_symbol, '_GrossRtn_Data.csv', sep = '')
  write.csv(gross_df, full_gross_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = gross_path, dataPath = gross_csv_path, dataSuffix = '_GrossRtn_Data.csv')
  }
  
  ######################################### Simple Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Simple Return TS Daily Calculations'))
  
  data_desc = "Simple Return"
  dir.create(file.path(anlysPath, "Simple_Return"), showWarnings = FALSE)
  simple_path = paste(anlysPath, "Simple_Return\\", sep="")
  
  num_lags = floor(length(tmp_simple_rtn_ts)/3)
  
  ExploreTimeData(timeData = tmp_simple_rtn_ts, dataSymbol = tmp_symbol, filePath = simple_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(simple_path, "Data"), showWarnings = FALSE)
  simple_csv_path = paste(simple_path, "Data\\", sep="")

  simple_df = data.frame(tmp_simple_rtn_ts)
  simple_df$date = rownames(simple_df)
  rownames(simple_df) <- 1:nrow(simple_df)
  
  full_simple_csv_path = paste(simple_csv_path, tmp_symbol, '_SimpleRtn_Data.csv', sep = '')
  write.csv(simple_df, full_simple_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = simple_path, dataPath = simple_csv_path, dataSuffix = '_SimpleRtn_Data.csv')
  }

  
  ########################################################################################################################
  #################################################### Monthly Data ######################################################
  ########################################################################################################################
  
  dir.create(file.path(mainAnlysPath, "Monthly"), showWarnings = FALSE)
  anlysPath = paste(mainAnlysPath, "Monthly\\", sep="")
  
  ########################################## Time Series Exploratory Data ################################################
  
  print(paste(tmp_symbol, 'TS Monthly Calculations'))

  tmp_data_ts_mon = aggregate(tmp_data_ts, as.yearmon, mean)
  
  data_desc = "Time Series"
  dir.create(file.path(anlysPath, "TS"), showWarnings = FALSE)
  ts_path = paste(anlysPath, "TS\\", sep="")
  
  num_lags = floor(length(tmp_data_ts_mon)/3)
  
  ExploreTimeData(timeData = tmp_data_ts_mon, dataSymbol = tmp_symbol, filePath = ts_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(ts_path, "Data"), showWarnings = FALSE)
  ts_csv_path = paste(ts_path, "Data\\", sep="")
  
  ts_df = data.frame(tmp_data_ts_mon)
  ts_df$date = rownames(ts_df)
  rownames(ts_df) <- 1:nrow(ts_df)
  
  full_ts_csv_path = paste(ts_csv_path, tmp_symbol, '_TS_Data.csv', sep = '')
  write.csv(ts_df, full_ts_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = ts_path, dataPath = ts_csv_path, dataSuffix = '_TS_Data.csv')
  }
  
  
  ####################################### Log Time Series Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Log TS Monthly Calculations'))
  
  log_data_ts_mon = aggregate(log_data_ts, as.yearmon, mean)
  
  data_desc = "Log Time Series"
  dir.create(file.path(anlysPath, "Log_TS"), showWarnings = FALSE)
  log_path = paste(anlysPath, "Log_TS\\", sep="")
  
  num_lags = floor(length(log_data_ts_mon)/3)
  
  ExploreTimeData(timeData = log_data_ts_mon, dataSymbol = tmp_symbol, filePath = log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(log_path, "Data"), showWarnings = FALSE)
  log_csv_path = paste(log_path, "Data\\", sep="")
  
  log_df = data.frame(log_data_ts_mon)
  log_df$date = rownames(log_df)
  rownames(log_df) <- 1:nrow(log_df)
  
  full_log_csv_path = paste(log_csv_path, tmp_symbol, '_Log_Data.csv', sep = '')
  write.csv(log_df, full_log_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = log_path, dataPath = log_csv_path, dataSuffix = '_Log_Data.csv')
  }
  
  ####################################### Diff Time Series Exploratory Data ##############################################
  
  print(paste(tmp_symbol, 'Diff TS Monthly Calculations'))
  
  diff_data_ts_mon = aggregate(diff_data_ts, as.yearmon, mean)
  
  data_desc = "Diff Time Series"
  dir.create(file.path(anlysPath, "Diff_TS"), showWarnings = FALSE)
  diff_path = paste(anlysPath, "Diff_TS\\", sep="")
  
  num_lags = floor(length(diff_data_ts_mon)/3)
  
  ExploreTimeData(timeData = diff_data_ts_mon, dataSymbol = tmp_symbol, filePath = diff_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_path, "Data"), showWarnings = FALSE)
  diff_csv_path = paste(diff_path, "Data\\", sep="")
  
  diff_df = data.frame(diff_data_ts_mon)
  diff_df$date = rownames(diff_df)
  rownames(diff_df) <- 1:nrow(diff_df)
  
  full_diff_csv_path = paste(diff_csv_path, tmp_symbol, '_Diff_Data.csv', sep = '')
  write.csv(diff_df, full_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_path, dataPath = diff_csv_path, dataSuffix = '_Diff_Data.csv')
  }
  
  ###################################### Log Diff Time Series Exploratory Data ###########################################
  
  print(paste(tmp_symbol, 'Diff Log TS Monthly Calculations'))
  
  diff_log_data_mon = aggregate(diff_log_data_ts, as.yearmon, mean)
  
  data_desc = "Diff Log Time Series"
  dir.create(file.path(anlysPath, "Diff_Log_TS"), showWarnings = FALSE)
  diff_log_path = paste(anlysPath, "Diff_Log_TS\\", sep="")
  
  num_lags = floor(length(diff_log_data_mon)/3)
  
  ExploreTimeData(timeData = diff_log_data_mon, dataSymbol = tmp_symbol, filePath = diff_log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_log_path, "Data"), showWarnings = FALSE)
  log_diff_csv_path = paste(diff_log_path, "Data\\", sep="")
  
  log_diff_df = data.frame(diff_log_data_mon)
  log_diff_df$date = rownames(log_diff_df)
  rownames(log_diff_df) <- 1:nrow(log_diff_df)
  
  full_log_diff_csv_path = paste(log_diff_csv_path, tmp_symbol, '_LogDiff_Data.csv', sep = '')
  write.csv(log_diff_df, full_log_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_log_path, dataPath = log_diff_csv_path, dataSuffix = '_LogDiff_Data.csv')
  }
  
  ########################################## Gross Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Gross Return TS Monthly Calculations'))
  
  tmp_gross_rtn_ts_mon = aggregate(tmp_gross_rtn_ts, as.yearmon, mean)
  
  data_desc = "Gross Return"
  dir.create(file.path(anlysPath, "Gross_Return"), showWarnings = FALSE)
  gross_path = paste(anlysPath, "Gross_Return\\", sep="")
  
  num_lags = floor(length(tmp_gross_rtn_ts_mon)/3)
  
  ExploreTimeData(timeData = tmp_gross_rtn_ts_mon, dataSymbol = tmp_symbol, filePath = gross_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(gross_path, "Data"), showWarnings = FALSE)
  gross_csv_path = paste(gross_path, "Data\\", sep="")
  
  gross_df = data.frame(tmp_gross_rtn_ts_mon)
  gross_df$date = rownames(gross_df)
  rownames(gross_df) <- 1:nrow(gross_df)
  
  full_gross_csv_path = paste(gross_csv_path, tmp_symbol, '_GrossRtn_Data.csv', sep = '')
  write.csv(gross_df, full_gross_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = gross_path, dataPath = gross_csv_path, dataSuffix = '_GrossRtn_Data.csv')
  }
  
  ######################################### Simple Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Simple Return TS Monthly Calculations'))
  
  tmp_simple_rtn_ts_mon = aggregate(tmp_simple_rtn_ts, as.yearmon, mean)
  
  data_desc = "Simple Return"
  dir.create(file.path(anlysPath, "Simple_Return"), showWarnings = FALSE)
  simple_path = paste(anlysPath, "Simple_Return\\", sep="")
  
  num_lags = floor(length(tmp_simple_rtn_ts_mon)/3)
  
  ExploreTimeData(timeData = tmp_simple_rtn_ts_mon, dataSymbol = tmp_symbol, filePath = simple_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(simple_path, "Data"), showWarnings = FALSE)
  simple_csv_path = paste(simple_path, "Data\\", sep="")
  
  simple_df = data.frame(tmp_simple_rtn_ts_mon)
  simple_df$date = rownames(simple_df)
  rownames(simple_df) <- 1:nrow(simple_df)
  
  full_simple_csv_path = paste(simple_csv_path, tmp_symbol, '_SimpleRtn_Data.csv', sep = '')
  write.csv(simple_df, full_simple_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = simple_path, dataPath = simple_csv_path, dataSuffix = '_SimpleRtn_Data.csv')
  }
  
  ########################################################################################################################
  ##################################################### Weekly Data ######################################################
  ########################################################################################################################
  
  ################################################ Aggregate Weekly Data #################################################
  
  diff_dates = diff(tmp_dates)
  
  entry_ndx = 1
  day_cnt = 0
  week_sum = 0
  week_avg = c()
  wk_num_days = c()
  week_data = c()
  week_dates = c()
  num_pts = length(diff_dates)
  curr_date = c()
  for (x in 1:num_pts){
    
    week_sum = week_sum + tmp_data$close[x]
    day_cnt = day_cnt + 1
    
      if (diff_dates[x] > 2){
      
      week_data[entry_ndx] = week_sum
      wk_num_days[entry_ndx] = day_cnt
      week_avg[entry_ndx] = week_sum / day_cnt
      
      if (entry_ndx == 1){
        curr_date = as.Date(tmp_data$date[x])
        week_dates[entry_ndx] = as.character(curr_date)
      }else{
        curr_date = curr_date + 7
        week_dates[entry_ndx] = as.character(curr_date)
      }
      
      entry_ndx = entry_ndx + 1
      week_sum = 0
      day_cnt = 0
      
      
    } else if (x == num_pts){
      
      week_sum = week_sum + tmp_data$close[x + 1]
      day_cnt = day_cnt + 1
      
      week_data[entry_ndx] = week_sum
#      week_dates[entry_ndx] = tmp_data$date[i + 1]
      wk_num_days[entry_ndx] = day_cnt
      week_avg[entry_ndx] = week_sum / day_cnt
      
      curr_date = curr_date + 7
      week_dates[entry_ndx] = as.character(curr_date)
      
      
    } 
    
  }
  
  tmp_wk_dates = as.Date(week_dates)
  
  num_wk_rows = length(week_avg)
  tmp_short_wk_dates = as.Date(week_dates[2:num_wk_rows])
  
  ########################################################################################################################
  
  dir.create(file.path(mainAnlysPath, "Weekly"), showWarnings = FALSE)
  anlysPath = paste(mainAnlysPath, "Weekly\\", sep="")
  
  ########################################## Time Series Exploratory Data ################################################
  
  print(paste(tmp_symbol, 'TS Weekly Calculations'))
  
  wk_tmp_data_ts = zoo(week_avg, tmp_wk_dates)
  
  data_desc = "Time Series"
  dir.create(file.path(anlysPath, "TS"), showWarnings = FALSE)
  ts_path = paste(anlysPath, "TS\\", sep="")
  
  num_lags = floor(length(wk_tmp_data_ts)/3)
  
  ExploreTimeData(timeData = wk_tmp_data_ts, dataSymbol = tmp_symbol, filePath = ts_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(ts_path, "Data"), showWarnings = FALSE)
  ts_csv_path = paste(ts_path, "Data\\", sep="")
  
  ts_df = data.frame(wk_tmp_data_ts)
  ts_df$date = rownames(ts_df)
  rownames(ts_df) <- 1:nrow(ts_df)
  
  full_ts_csv_path = paste(ts_csv_path, tmp_symbol, '_TS_Data.csv', sep = '')
  write.csv(ts_df, full_ts_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = ts_path, dataPath = ts_csv_path, dataSuffix = '_TS_Data.csv')
  }
  
  
  ####################################### Log Time Series Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Log TS Weekly Calculations'))
  
  wk_log_data = log(week_avg)
  wk_log_data_ts = zoo(wk_log_data, tmp_wk_dates)
  
  data_desc = "Log Time Series"
  dir.create(file.path(anlysPath, "Log_TS"), showWarnings = FALSE)
  log_path = paste(anlysPath, "Log_TS\\", sep="")
  
  num_lags = floor(length(wk_log_data_ts)/3)
  
  ExploreTimeData(timeData = wk_log_data_ts, dataSymbol = tmp_symbol, filePath = log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(log_path, "Data"), showWarnings = FALSE)
  log_csv_path = paste(log_path, "Data\\", sep="")
  
  log_df = data.frame(wk_log_data_ts)
  log_df$date = rownames(log_df)
  rownames(log_df) <- 1:nrow(log_df)
  
  full_log_csv_path = paste(log_csv_path, tmp_symbol, '_Log_Data.csv', sep = '')
  write.csv(log_df, full_log_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = log_path, dataPath = log_csv_path, dataSuffix = '_Log_Data.csv')
  }
  
  ####################################### Diff Time Series Exploratory Data ##############################################
  
  print(paste(tmp_symbol, 'Diff TS Weekly Calculations'))
  
  wk_diff_data = diff(week_avg)
  wk_diff_data_ts = zoo(wk_diff_data, tmp_short_wk_dates)
  
  data_desc = "Diff Time Series"
  dir.create(file.path(anlysPath, "Diff_TS"), showWarnings = FALSE)
  diff_path = paste(anlysPath, "Diff_TS\\", sep="")
  
  num_lags = floor(length(wk_diff_data_ts)/3)
  
  ExploreTimeData(timeData = wk_diff_data_ts, dataSymbol = tmp_symbol, filePath = diff_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_path, "Data"), showWarnings = FALSE)
  diff_csv_path = paste(diff_path, "Data\\", sep="")
  
  diff_df = data.frame(wk_diff_data_ts)
  diff_df$date = rownames(diff_df)
  rownames(diff_df) <- 1:nrow(diff_df)
  
  full_diff_csv_path = paste(diff_csv_path, tmp_symbol, '_Diff_Data.csv', sep = '')
  write.csv(diff_df, full_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_path, dataPath = diff_csv_path, dataSuffix = '_Diff_Data.csv')
  }
  
  ###################################### Log Diff Time Series Exploratory Data ###########################################
  
  print(paste(tmp_symbol, 'Diff Log TS Weekly Calculations'))
  
  wk_diff_log_data = diff(log(week_avg))
  wk_diff_log_data_ts = zoo(wk_diff_log_data, tmp_short_wk_dates)
  
  data_desc = "Diff Log Time Series"
  dir.create(file.path(anlysPath, "Diff_Log_TS"), showWarnings = FALSE)
  diff_log_path = paste(anlysPath, "Diff_Log_TS\\", sep="")
  
  num_lags = floor(length(wk_diff_log_data_ts)/3)
  
  ExploreTimeData(timeData = wk_diff_log_data_ts, dataSymbol = tmp_symbol, filePath = diff_log_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(diff_log_path, "Data"), showWarnings = FALSE)
  log_diff_csv_path = paste(diff_log_path, "Data\\", sep="")
  
  log_diff_df = data.frame(wk_diff_log_data_ts)
  log_diff_df$date = rownames(log_diff_df)
  rownames(log_diff_df) <- 1:nrow(log_diff_df)
  
  full_log_diff_csv_path = paste(log_diff_csv_path, tmp_symbol, '_LogDiff_Data.csv', sep = '')
  write.csv(log_diff_df, full_log_diff_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = diff_log_path, dataPath = log_diff_csv_path, dataSuffix = '_LogDiff_Data.csv')
  }
  
  ############################################### Return Data Creation ###################################################
  
  tmp_gross_rtn = rep(1, num_wk_rows)
  tmp_simple_rtn = rep(1, num_wk_rows)
  
  for (j in 1:num_rows){
    
    if (j == 1){
      tmp_gross_rtn[j] = NA
      tmp_simple_rtn[j] = NA
    }
    else{
      tmp_gross_rtn[j] = week_avg[j] / week_avg[(j-1)]
      tmp_simple_rtn[j] = (week_avg[j] / week_avg[(j-1)]) - 1
    }
  }
  
  wk_gross_rtn = tmp_gross_rtn[2:num_rows]
  wk_simple_rtn = tmp_simple_rtn[2:num_rows]
  
  tmp_wk_gross_rtn_ts = zoo(wk_gross_rtn, tmp_short_wk_dates)
  tmp_wk_simple_rtn_ts = zoo(wk_simple_rtn, tmp_short_wk_dates)
  
  ########################################## Gross Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Gross Return TS Weekly Calculations'))
  
  data_desc = "Gross Return"
  dir.create(file.path(anlysPath, "Gross_Return"), showWarnings = FALSE)
  gross_path = paste(anlysPath, "Gross_Return\\", sep="")
  
  num_lags = floor(length(tmp_wk_gross_rtn_ts)/3)
  
  ExploreTimeData(timeData = tmp_wk_gross_rtn_ts, dataSymbol = tmp_symbol, filePath = gross_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(gross_path, "Data"), showWarnings = FALSE)
  gross_csv_path = paste(gross_path, "Data\\", sep="")
  
  gross_df = data.frame(tmp_wk_gross_rtn_ts)
  gross_df$date = rownames(gross_df)
  rownames(gross_df) <- 1:nrow(gross_df)
  
  full_gross_csv_path = paste(gross_csv_path, tmp_symbol, '_GrossRtn_Data.csv', sep = '')
  write.csv(gross_df, full_gross_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = gross_path, dataPath = gross_csv_path, dataSuffix = '_GrossRtn_Data.csv')
  }
  
  ######################################### Simple Return Exploratory Data ###############################################
  
  print(paste(tmp_symbol, 'Simple Return TS Weekly Calculations'))
  
  data_desc = "Simple Return"
  dir.create(file.path(anlysPath, "Simple_Return"), showWarnings = FALSE)
  simple_path = paste(anlysPath, "Simple_Return\\", sep="")
  
  num_lags = floor(length(tmp_wk_simple_rtn_ts)/3)
  
  ExploreTimeData(timeData = tmp_wk_simple_rtn_ts, dataSymbol = tmp_symbol, filePath = simple_path, plotDesc = data_desc, numLags = 5)
  
  dir.create(file.path(simple_path, "Data"), showWarnings = FALSE)
  simple_csv_path = paste(simple_path, "Data\\", sep="")
  
  simple_df = data.frame(tmp_wk_simple_rtn_ts)
  simple_df$date = rownames(simple_df)
  rownames(simple_df) <- 1:nrow(simple_df)
  
  full_simple_csv_path = paste(simple_csv_path, tmp_symbol, '_SimpleRtn_Data.csv', sep = '')
  write.csv(simple_df, full_simple_csv_path)
  
  if (i == num_stocks){
    CrossCorrTimeData(allSymbols = stock_syms, plotPath = simple_path, dataPath = simple_csv_path, dataSuffix = '_SimpleRtn_Data.csv')
  }
  
}

