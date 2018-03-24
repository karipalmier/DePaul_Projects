library(zoo)
library(tseries)
library(fBasics)
library(fUnitRoots)
library(lmtest)
library(forecast)


# Get the path of the current based on if RStudio is run or if run from command line
cmdArgs <- commandArgs(trailingOnly = FALSE)

# If run from RStudio, use the rstudio api to get the path of the current R script
mainPath = dirname(rstudioapi::getSourceEditorContext()$path)
mainPath = gsub("/", "\\\\", mainPath)
mainPath = paste(mainPath, '\\', sep="")

source(paste(mainPath, "backtest.R", sep=""))
source(paste(mainPath, "ModelTimeData.R", sep=""))

dir.create(file.path(mainPath, "R_Output"), showWarnings = FALSE)
mainAnlysPath = paste(mainPath, "R_Output\\", sep="")

dir.create(file.path(mainAnlysPath, "Modeling"), showWarnings = FALSE)
mainAnlysPath = paste(mainAnlysPath, "Modeling\\", sep="")

# Read in Apple weekly simple return data
apple_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\AAPL_SimpleRtn_Data.csv', 
                       header = T, sep = ',')
apple_data$date = as.character(apple_data$date)
apple_data$date = as.Date(apple_data$date)

apple_ts = zoo(apple_data$tmp_wk_simple_rtn_ts, apple_data$date)

out_file_name = paste(mainAnlysPath, 'Apple_SimpleRtn_AutoArima.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)

# Basic Statistics
print('*** Auto Arima BIC Criterion Results ***')
print(auto.arima(apple_ts, ic = c("bic")))
print("", quote=FALSE)

sink()

close(outFile)
closeAllConnections()

model_bics = c()
model_names = c()

#################################################### MA (1) Model ########################################################
# Model recommended by auto.arima

ma1_orders = c(0,0,1)
ma1_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), FALSE, NULL, mainAnlysPath, 'Apple_SimpleRtn')
model_bics[1] = ma1_model$bic
model_names[1] = 'Apple_SimpleRtn_MA1'

#################################################### ARMA (1) Model ######################################################

arma1_orders = c(1,0,1)
arma1_model = ModelTimeData(apple_ts, arma1_orders, 0, c(), FALSE, NULL, mainAnlysPath, 'Apple_SimpleRtn')
model_bics[2] = arma1_model$bic
model_names[2] = 'Apple_SimpleRtn_ARMA1'

#################################################### AR (1) Model ########################################################

ar1_orders = c(1,0,0)
ar1_model = ModelTimeData(apple_ts, ar1_orders, 0, c(), FALSE, NULL, mainAnlysPath, 'Apple_SimpleRtn')
model_bics[3] = ar1_model$bic
model_names[3] = 'Apple_SimpleRtn_AR1'

#################################################### X Regressors ########################################################

csco_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\CSCO_SimpleRtn_Data.csv', 
                        header = T, sep = ',')
csco_data$date = as.character(csco_data$date)
csco_data$date = as.Date(csco_data$date)
csco_ts = zoo(csco_data$tmp_wk_simple_rtn_ts, csco_data$date)

goog_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\GOOG_SimpleRtn_Data.csv', 
                       header = T, sep = ',')
goog_data$date = as.character(goog_data$date)
goog_data$date = as.Date(goog_data$date)
goog_ts = zoo(goog_data$tmp_wk_simple_rtn_ts, goog_data$date)

googl_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\GOOGL_SimpleRtn_Data.csv', 
                       header = T, sep = ',')
googl_data$date = as.character(googl_data$date)
googl_data$date = as.Date(googl_data$date)
googl_ts = zoo(googl_data$tmp_wk_simple_rtn_ts, googl_data$date)

orcl_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\ORCL_SimpleRtn_Data.csv', 
                        header = T, sep = ',')
orcl_data$date = as.character(orcl_data$date)
orcl_data$date = as.Date(orcl_data$date)
orcl_ts = zoo(orcl_data$tmp_wk_simple_rtn_ts, orcl_data$date)

rht_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\RHT_SimpleRtn_Data.csv', 
                       header = T, sep = ',')
rht_data$date = as.character(rht_data$date)
rht_data$date = as.Date(rht_data$date)
rht_ts = zoo(rht_data$tmp_wk_simple_rtn_ts, rht_data$date)

txn_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\TXN_SimpleRtn_Data.csv', 
                      header = T, sep = ',')
txn_data$date = as.character(txn_data$date)
txn_data$date = as.Date(txn_data$date)
txn_ts = zoo(txn_data$tmp_wk_simple_rtn_ts, txn_data$date)

yhoo_data = read.table('C:\\DePaulCoursework\\Winter 2018 CSC 425\\Project\\R_Output\\Exploratory\\Weekly\\Simple_Return\\Data\\YHOO_SimpleRtn_Data.csv', 
                      header = T, sep = ',')
yhoo_data$date = as.character(yhoo_data$date)
yhoo_data$date = as.Date(yhoo_data$date)
yhoo_ts = zoo(yhoo_data$tmp_wk_simple_rtn_ts, yhoo_data$date)

tmp_pair = paste(mainAnlysPath, 'Apple_SimpleRtn_ScatterMatrix.jpg', sep = '')
jpeg(file = tmp_pair)
pairs(~apple_ts + csco_ts + goog_ts + googl_ts + orcl_ts + rht_ts + txn_ts + yhoo_ts)
dev.off()

############################################## MA (1) All X Regressors Model #############################################

xreg_data = data.frame(csco_ts, goog_ts, googl_ts, orcl_ts, rht_ts, txn_ts, yhoo_ts)

ma1_orders = c(0,0,1)
ma1_all_xregs_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_All')
model_bics[4] = ma1_all_xregs_model$bic
model_names[4] = 'Apple_SimpleRtn_All'

################################################### MA (1) No GOOGL Model ################################################

xreg_data = data.frame(csco_ts, goog_ts, orcl_ts, rht_ts, txn_ts, yhoo_ts)

ma1_orders = c(0,0,1)
ma1_no_googl_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_No_GOOGL')
model_bics[5] = ma1_no_googl_model$bic
model_names[5] = 'Apple_SimpleRtn_No_GOOGL'

################################################ MA (1) No GOOGL, RHT Model ##############################################

xreg_data = data.frame(csco_ts, goog_ts, orcl_ts, txn_ts, yhoo_ts)

ma1_orders = c(0,0,1)
ma1_no_googl_rht_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_No_GOOGL_RHT')
model_bics[6] = ma1_no_googl_rht_model$bic
model_names[6] = 'Apple_SimpleRtn_No_GOOGL_RHT'

############################################## MA (1) No GOOGL, RHT, YHOO Model ##########################################

xreg_data = data.frame(csco_ts, goog_ts, orcl_ts, txn_ts)

ma1_orders = c(0,0,1)
ma1_no_googl_rht_yhoo_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_No_GOOGL_RHT_YHOO')
model_bics[7] = ma1_no_googl_rht_yhoo_model$bic
model_names[7] = 'Apple_SimpleRtn_No_GOOGL_RHT_YHOO'

############################################### MA (1) W GOOG, ORCL, TXN Model ###########################################

xreg_data = data.frame(goog_ts, orcl_ts, txn_ts)

ma1_orders = c(0,0,1)
ma1_w_goog_orcl_txn_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_GOOG_ORCL_TXN')
model_bics[8] = ma1_w_goog_orcl_txn_model$bic
model_names[8] = 'Apple_SimpleRtn_W_GOOG_ORCL_TXN'

################################################# MA (1) W GOOG, TXN Model ###############################################

xreg_data = data.frame(goog_ts, txn_ts)

ma1_orders = c(0,0,1)
ma1_w_goog_txn_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_GOOG_TXN')
model_bics[9] = ma1_w_goog_txn_model$bic
model_names[9] = 'Apple_SimpleRtn_W_GOOG_TXN'

################################################# MA (1) W GOOG, ORCL Model ##############################################

xreg_data = data.frame(goog_ts, orcl_ts)

ma1_orders = c(0,0,1)
ma1_w_goog_orcl_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_GOOG_ORCL')
model_bics[10] = ma1_w_goog_orcl_model$bic
model_names[10] = 'Apple_SimpleRtn_W_GOOG_ORCL'

################################################# MA (1) W TXN, ORCL Model ###############################################

xreg_data = data.frame(txn_ts, orcl_ts)

ma1_orders = c(0,0,1)
ma1_w_txn_orcl_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_TXN_ORCL')
model_bics[11] = ma1_w_txn_orcl_model$bic
model_names[11] = 'Apple_SimpleRtn_W_TXN_ORCL'

################################################# MA (1) W GOOG Model ####################################################

xreg_data = data.frame(goog_ts)

ma1_orders = c(0,0,1)
ma1_w_goog_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_GOOG')
model_bics[12] = ma1_w_goog_model$bic
model_names[12] = 'Apple_SimpleRtn_W_GOOG'

################################################# MA (1) W TXN Model #####################################################

xreg_data = data.frame(txn_ts)

ma1_orders = c(0,0,1)
ma1_w_txn_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_TXN')
model_bics[13] = ma1_w_txn_model$bic
model_names[13] = 'Apple_SimpleRtn_W_TXN'

################################################# MA (1) W ORCL Model ####################################################

xreg_data = data.frame(orcl_ts)

ma1_orders = c(0,0,1)
ma1_w_orcl_model = ModelTimeData(apple_ts, ma1_orders, 0, c(), TRUE, xreg_data, mainAnlysPath, 'Apple_SimpleRtn_W_ORCL')
model_bics[14] = ma1_w_orcl_model$bic
model_names[14] = 'Apple_SimpleRtn_W_ORCL'

############################################### Find the Best Model BIC ##################################################

max_bic = min(model_bics)
min_ndx = model_bics == max_bic

out_file_name = paste(mainAnlysPath, 'Apple_SimpleRtn_Model_BIC_Summary.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)

print('Model With Lowest BIC:')
print(model_names[min_ndx])
print(paste('BIC = ', model_bics[min_ndx]))
print("", quote=FALSE)

print('All Model Information:')
bic_df = data.frame(model_names, model_bics)
print(bic_df)

sink()

close(outFile)
closeAllConnections()

############################### Forecast 10 Points into future with best (MA(1)) model ###################################

f_ma1=forecast(ma1_model, h=10)

out_file_name = paste(mainAnlysPath, 'Apple_SimpleRtn_MA1_Model_H10_Forecast.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)
print('Forecast of Apple Simple Return MA(1) Model 10 Points Into The Future:')
print(f_ma1)

sink()

close(outFile)
closeAllConnections()

tmp_ma1_bt_scatter = paste(mainAnlysPath, 'Apple_SimpleRtn_MA1_Model_H10_Forecast.jpg', sep = '')
jpeg(file = tmp_ma1_bt_scatter)
y_title = 'Simple Return Values'
plot_title = 'Forecasts from MA(1) Model of Apple Simple Returns'
plot(f_ma1, include=100, ylab = y_title, main = plot_title) 
lines(c(f_ma1$fitted, f_ma1$mean), col="blue")

dev.off()

############################## Forecast 10 Points into future with best (ARMA(1)) model ##################################

f_arma1=forecast(arma1_model, h=10)

out_file_name = paste(mainAnlysPath, 'Apple_SimpleRtn_ARMA1_Model_H10_Forecast.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)
print('Forecast of Apple Simple Return ARMA(1) Model 10 Points Into The Future:')
print(f_arma1)

sink()

close(outFile)
closeAllConnections()

tmp_arma1_bt_scatter = paste(mainAnlysPath, 'Apple_SimpleRtn_ARMA1_Model_H10_Forecast.jpg', sep = '')
jpeg(file = tmp_arma1_bt_scatter)
y_title = 'Simple Return Values'
plot_title = 'Forecasts from ARMA(1) Model of Apple Simple Returns'
plot(f_arma1, include=100, ylab = y_title, main = plot_title) 
lines(c(f_arma1$fitted, f_arma1$mean), col="blue")

dev.off()

############################# Forecast 10 Points into future with best (AR(1)) model ####################################

f_ar1=forecast(arma1_model, h=10)

out_file_name = paste(mainAnlysPath, 'Apple_SimpleRtn_AR1_Model_H10_Forecast.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)
print('Forecast of Apple Simple Return AR(1) Model 10 Points Into The Future:')
print(f_ar1)

sink()

close(outFile)
closeAllConnections()

tmp_ar1_bt_scatter = paste(mainAnlysPath, 'Apple_SimpleRtn_AR1_Model_H10_Forecast.jpg', sep = '')
jpeg(file = tmp_ar1_bt_scatter)
y_title = 'Simple Return Values'
plot_title = 'Forecasts from AR(1) Model of Apple Simple Returns'
plot(f_ar1, include=100, ylab = y_title, main = plot_title) 
lines(c(f_ar1$fitted, f_ar1$mean), col="blue")

dev.off()

############################################## Apple Squared ACF Plot ####################################################

apple_data$sq_wk_simple_rtn = (apple_data$tmp_wk_simple_rtn_ts)^2
apple_sq_ts = zoo(apple_data$sq_wk_simple_rtn, apple_data$date)

tmp_appl_sq_acf = paste(mainAnlysPath, 'AAPL_Squared_ACF.jpg', sep = '')
jpeg(file = tmp_appl_sq_acf)
plot_title = 'AAPL Simple Return Squared'
acf(apple_sq_ts, plot = TRUE, na.action = na.exclude, main = plot_title)
dev.off()

tmp_sq_scatter = paste(mainAnlysPath, 'AAPL_Squared_Scatter.jpg', sep = '')
jpeg(file = tmp_sq_scatter)
x_title = 'Date'
y_title = 'AAPL Simple Returns Squared'
plot_title = 'AAPL Simple Return Squared Vs. Time'
plot(apple_sq_ts, xlab = x_title, ylab = y_title, main = plot_title)
dev.off()

############################################## Apple Absolute ACF Plot ###################################################

apple_data$abs_wk_simple_rtn = abs(apple_data$tmp_wk_simple_rtn_ts)
apple_abs_ts = zoo(apple_data$abs_wk_simple_rtn, apple_data$date)

tmp_appl_abs_acf = paste(mainAnlysPath, 'AAPL_Abs_ACF.jpg', sep = '')
jpeg(file = tmp_appl_abs_acf)
plot_title = 'AAPL Absolute Simple Return'
acf(apple_abs_ts, plot = TRUE, na.action = na.exclude, main = plot_title)
dev.off()

tmp_abs_scatter = paste(mainAnlysPath, 'AAPL_Abs_Scatter.jpg', sep = '')
jpeg(file = tmp_abs_scatter)
x_title = 'Date'
y_title = 'AAPL Absolute of Simple Returns'
plot_title = 'AAPL Absolute of Simple Return Vs. Time'
plot(apple_sq_ts, xlab = x_title, ylab = y_title, main = plot_title)
dev.off()

############################################ Apple ARCH Ljung Box Test ###################################################

out_file_name = paste(mainAnlysPath, 'Apple_ARCH_LjungBox.txt', sep = '')
outFile = file(out_file_name, open="wt")

sink(file = outFile, append = TRUE)

print('Ljung Box test for Squared Simple Returns for 5 Lags:')
print(Box.test(apple_sq_ts, lag = 5, type = 'Ljung'))
print("", quote=FALSE)

print('Ljung Box test for Absolute of Simple Returns for 5 Lags:')
print(Box.test(apple_abs_ts, lag = 5, type = 'Ljung'))
print("", quote=FALSE)
print("", quote=FALSE)

print('Ljung Box test for Squared Simple Returns for 10 Lags:')
print(Box.test(apple_sq_ts, lag = 10, type = 'Ljung'))
print("", quote=FALSE)

print('Ljung Box test for Absolute of Simple Returns for 10 Lags:')
print(Box.test(apple_abs_ts, lag = 10, type = 'Ljung'))
print("", quote=FALSE)
print("", quote=FALSE)

print('Ljung Box test for Squared Simple Returns for 25 Lags:')
print(Box.test(apple_sq_ts, lag = 25, type = 'Ljung'))
print("", quote=FALSE)

print('Ljung Box test for Absolute of Simple Returns for 25 Lags:')
print(Box.test(apple_abs_ts, lag = 25, type = 'Ljung'))
print("", quote=FALSE)

sink()

close(outFile)
closeAllConnections()






