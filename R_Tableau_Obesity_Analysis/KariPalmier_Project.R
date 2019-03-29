##################################################################################################################################
#
#  Kari Palmier CSC 465 Final Project
#
##################################################################################################################################

library(ggplot2)
library(lubridate)
library(dplyr)
library(mosaic)
library(scales)
library(psych)
library(gridExtra)

##################################################################################################################################
#
#  Initial Exploratory Analysis
#
##################################################################################################################################

nhanesData = read.table("C:\\DePaul Coursework\\Fall 2017 CSC 465\\Final Project\\NHANES_ConbinedProjectDataset.csv", sep=",", header=T)
nhanesData$SEQN = nhanesData$ï..SEQN
nhanesData$ï..SEQN = NULL

# Print summary of original data
nrow(nhanesData)
summary(nhanesData)
describe(nhanesData)
colSums(is.na(nhanesData))
head(nhanesData)
str(nhanesData)

  
##################################################################################################################################
#
#  Initial Exploratory Histograms For Variables Of Interest 
#
##################################################################################################################################

ggplot(data = nhanesData, aes(x = RIAGENDR)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 3)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Gender Counts") + 
  labs(x = "Gender", y = "Count")

ggplot(data = nhanesData, aes(x = RIDAGEYR)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 81)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Age Histogram") + 
  labs(x = "Age (Years)", y = "Frequency")

ggplot(data = nhanesData, aes(x = DMDEDUC2)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 6)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Education Counts") + 
  labs(x = "Education", y = "Count")

ggplot(data = nhanesData, aes(x = DMDMARTL)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 7)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Marital Status Counts") + 
  labs(x = "Marital Status Levels", y = "Count")

ggplot(data = nhanesData, aes(x = RIDRETH1)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 6)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Race Counts") + 
  labs(x = "Races", y = "Count")

ggplot(data = nhanesData, aes(x = INDHHIN2)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 16)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Income Counts") + 
  labs(x = "Income", y = "Count")

ggplot(data = nhanesData, aes(x = BPQ020)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 3)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Hypertension Counts") + 
  labs(x = "Hypertension Levels", y = "Count")

ggplot(data = nhanesData, aes(x = WHD010)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(47, 82)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Height Histogram") + 
  labs(x = "Height (in)", y = "Frequency")

ggplot(data = nhanesData, aes(x = WHD020)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(74, 494)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Weight Histogram") + 
  labs(x = "Weight (lbs)", y = "Frequency")

ggplot(data = nhanesData, aes(x = BPQ020)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 4)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Initial Diabetes Counts") + 
  labs(x = "Diabetes Levels", y = "Count")


##################################################################################################################################
#
#  Data cleaning
#
##################################################################################################################################

attrs = colnames(nhanesData)
numAttrs = length(attrs)

refusedEntries = c(0, 0, 0, 7, 77, 77, 0, 7, 777777, 777777, 777777, 777777, 7, 7777, 7777, 7777, 7777, 7, 7, 7, 7777, 7, 7777, 7777, 0)
dkEntries = c(0, 0, 0, 9, 99, 99, 0, 9, 999999, 999999, 999999, 999999, 9, 9999, 9999, 9999, 9999, 9, 9, 9, 9999, 9, 9999, 9999, 0)


# Remove NaN values
for (i in 1:numAttrs){
  current_attr = attrs[i]
  notNaNdx = !is.na(nhanesData[current_attr])
  nhanesData = nhanesData[notNaNdx,]
  
  print(i)
  print(current_attr)
  print('After NaN Removal')
  print(nrow(nhanesData))
  
}

# Remove refused values
for (i in 1:numAttrs){
  current_attr = attrs[i]
  
  refNdx = nhanesData[current_attr] == refusedEntries[i]
  if (any(refNdx)){
    nhanesData = nhanesData[!refNdx,]
  }
  
}

# Remove don't know values
for (i in 1:numAttrs){
  current_attr = attrs[i]
  
  dkNdx = nhanesData[current_attr] == dkEntries[i]
  if (any(dkNdx)){
    nhanesData = nhanesData[!dkNdx,]
  }
  
}

# Remove income values for $20,000 and over (12) and under $20,000 because the majority of data was not in these bins
# Also because having a range like this and bins that overlap the range will lead to issues using the variable
twelveNdx = nhanesData$INDHHIN2 == 12
if (any(twelveNdx)){
  nhanesData = nhanesData[!twelveNdx,]
}


thirteenNdx = nhanesData$INDHHIN2 == 13
if (any(thirteenNdx)){
  nhanesData = nhanesData[!thirteenNdx,]
}


##################################################################################################################################
#
# Exploratory Histograms For Variables Of Interest After Data Cleaning (To Ensure Distributions are still maintained)
#
##################################################################################################################################

ggplot(data = nhanesData, aes(x = RIAGENDR)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 3)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Gender Counts") + 
  labs(x = "Gender", y = "Count")

ggplot(data = nhanesData, aes(x = RIDAGEYR)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 81)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Age Histogram") + 
  labs(x = "Age (Years)", y = "Frequency")

ggplot(data = nhanesData, aes(x = DMDEDUC2)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 6)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Education Counts") + 
  labs(x = "Education", y = "Count")

ggplot(data = nhanesData, aes(x = DMDMARTL)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 7)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Marital Status Counts") + 
  labs(x = "Marital Status Levels", y = "Count")

ggplot(data = nhanesData, aes(x = RIDRETH1)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 6)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Race Counts") + 
  labs(x = "Races", y = "Count")

ggplot(data = nhanesData, aes(x = INDHHIN2)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 16)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Income Counts") + 
  labs(x = "Income", y = "Count")

ggplot(data = nhanesData, aes(x = BPQ020)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 3)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Hypertension Counts") + 
  labs(x = "Hypertension Levels", y = "Count")

ggplot(data = nhanesData, aes(x = WHD010)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(47, 82)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Height Histogram") + 
  labs(x = "Height (in)", y = "Frequency")

ggplot(data = nhanesData, aes(x = WHD020)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(74, 494)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Weight Histogram") + 
  labs(x = "Weight (lbs)", y = "Frequency")

ggplot(data = nhanesData, aes(x = BPQ020)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 4)) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("After Cleaning Diabetes Counts") + 
  labs(x = "Diabetes Levels", y = "Count")


##################################################################################################################################
#
# Create New Variables
#
##################################################################################################################################

lb_to_kg_const =  0.45359237
ft_to_m_const = 0.3048

nhanesData$Weight_kg = nhanesData$WHD020 * lb_to_kg_const
nhanesData$Height_m = nhanesData$WHD010 * ft_to_m_const

nhanesData$BMI_Perc = (nhanesData$Weight_kg / (nhanesData$Height_m^2)) * 100

# Create ObeseFlag attribute for if person is obese (obesity is BMI >= 30)
nhanesData$ObeseFlag = rep(1, nrow(nhanesData))
notObeseNdx = nhanesData$BMI_Perc < 30
nhanesData$ObeseFlag[notObeseNdx] = 0

##################################################################################################################################
#
# Create Basic Exploratory Plots And Text File For All Vars
#
##################################################################################################################################

# Create path to save exploratory plots
explorePath = "C:\\DePaul Coursework\\Fall 2017 CSC 465\\Final Project\\Exploratory\\"

# Create output file
dir.create(explorePath, showWarnings = FALSE)
outFileName = paste(explorePath, "Explore_R_Output.txt", sep="")
outFile = file(outFileName, open="wt")

sumData = summary(nhanesData)
descData = describe(nhanesData)

# Print descriptive statistics
sink(file=outFile, append=TRUE)
print("Data Set Info:")
str(nhanesData)
print("", quote=FALSE)
print("Data Set Descriptive Statistics Summary:", quote=FALSE)
print("", quote=FALSE)
print(sumData, quote=FALSE)
print("", quote=FALSE)
print("Data Set Descriptive Statistics Describe Values:", quote=FALSE)
print("", quote=FALSE)
print(descData, quote=FALSE)
print("", quote=FALSE)
sink()

attrNames = colnames(nhanesData)
dir.create(file.path(explorePath, "Histograms"), showWarnings = FALSE)
newPath = paste(explorePath, "Histograms\\", sep="")
for (j in 1:length(attrNames)){
  # Plot Y Histogram
  fileName = paste(newPath, attrNames[j], "Hist.jpeg", sep="")
  plotTitle = paste(attrNames[j], "Histogram")
  jpeg(file=fileName)
  hData = c(t(nhanesData[attrNames[j]]))
  hist(hData, freq=TRUE, main=plotTitle, xlab=attrNames[j])
  dev.off()
}

dir.create(file.path(explorePath, "Box Plots"), showWarnings = FALSE)
newPath = paste(explorePath, "Box Plots\\", sep="")
for (i in 1:length(attrNames)){
  var = attrNames[i]
  fileName = paste(newPath, attrNames[i], "_BoxPlot.jpeg", sep="")
  plotTitle = paste(attrNames[i], " Box Plot")
  jpeg(file=fileName)
  plotData = c(t(nhanesData[var]))
  boxplot(plotData, main=plotTitle)
  dev.off()
}

# Create Correlation Values for all data
numAllVars = length(attrNames)
sink(file=outFile, append=TRUE)
print("", quote=FALSE)

for(i in 1:numAllVars){
  currentCor = cor(nhanesData[attrNames[i]], nhanesData[attrNames])
  
  assign("last.warning", NULL, envir = baseenv())
  
  print("", quote=FALSE)
  print(currentCor, quote=FALSE)
  print("", quote=FALSE)
  print("", quote=FALSE)
  
  assign("last.warning", NULL, envir = baseenv())
  
}  

sink()

close(outFile)
closeAllConnections()


##################################################################################################################################
#
# Convert nominal values to appropriate data types based on variable values from codebook
#
##################################################################################################################################
nhanesData$RIAGENDR = as.factor(nhanesData$RIAGENDR)
nhanesData$RIDRETH1 = as.factor(nhanesData$RIDRETH1)
nhanesData$DMDEDUC2 = as.factor(nhanesData$DMDEDUC2)
nhanesData$DMDMARTL = as.factor(nhanesData$DMDMARTL)
nhanesData$INDHHIN2 = as.factor(nhanesData$INDHHIN2)
nhanesData$BPQ020 = as.factor(nhanesData$BPQ020)
nhanesData$DIQ010 = as.factor(nhanesData$DIQ010)
nhanesData$MCQ080 = as.factor(nhanesData$MCQ080)
nhanesData$MCQ365A = as.factor(nhanesData$MCQ365A)
nhanesData$MCQ365B = as.factor(nhanesData$MCQ365B)
nhanesData$SMQ020 = as.factor(nhanesData$SMQ020)
nhanesData$ObeseFlag = as.factor(nhanesData$ObeseFlag)


##################################################################################################################################
#
#  Create factor attributes with descriptive strings for their levels for plotting
#
##################################################################################################################################

# Create Gender attribute with male and female for plotting
nhanesData$GenderStr = rep(1, nrow(nhanesData))
maleNdx = nhanesData$RIAGENDR == 1
femaleNdx = nhanesData$RIAGENDR == 2
nhanesData$GenderStr[maleNdx] = 'Male'
nhanesData$GenderStr[femaleNdx] = 'Female'

# Create Race attribute with race strings for plotting
nhanesData$RaceStr = rep(1, nrow(nhanesData))
mexAmNdx = nhanesData$RIDRETH1 == 1
othHispNdx = nhanesData$RIDRETH1 == 2
nonHispWNdx = nhanesData$RIDRETH1 == 3
nonHispBNdx = nhanesData$RIDRETH1 == 4
otherNdx = nhanesData$RIDRETH1 == 5
nhanesData$RaceStr[mexAmNdx] = 'Mexican American'
nhanesData$RaceStr[othHispNdx] = 'Other Hispanic'
nhanesData$RaceStr[nonHispWNdx] = 'Non-Hispanic White'
nhanesData$RaceStr[nonHispBNdx] = 'Non-Hispanic Black'
nhanesData$RaceStr[otherNdx] = 'Other'

# Create Income attibutes description strings for plotting
nhanesData$IncomeStr = rep(1, nrow(nhanesData))
ndx1 = nhanesData$INDHHIN2 == 1
ndx2 = nhanesData$INDHHIN2 == 2
ndx3 = nhanesData$INDHHIN2 == 3
ndx4 = nhanesData$INDHHIN2 == 4
ndx5 = nhanesData$INDHHIN2 == 5
ndx6 = nhanesData$INDHHIN2 == 6
ndx7 = nhanesData$INDHHIN2 == 7
ndx8 = nhanesData$INDHHIN2 == 8
ndx9 = nhanesData$INDHHIN2 == 9
ndx10 = nhanesData$INDHHIN2 == 10
ndx14 = nhanesData$INDHHIN2 == 14
ndx15 = nhanesData$INDHHIN2 == 15

nhanesData$IncomeStr[ndx1] = '$0 to $4,999'
nhanesData$IncomeStr[ndx2] = '$5,000 to $9,999'
nhanesData$IncomeStr[ndx3] = '$10,000 to $14,999'
nhanesData$IncomeStr[ndx4] = '$15,000 to $19,999'
nhanesData$IncomeStr[ndx5] = '$20,000 to $24,999'
nhanesData$IncomeStr[ndx6] = '$25,000 to $34,999'
nhanesData$IncomeStr[ndx7] = '$35,000 to $44,999'
nhanesData$IncomeStr[ndx8] = '$45,000 to $54,999'
nhanesData$IncomeStr[ndx9] = '$55,000 to $64,999'
nhanesData$IncomeStr[ndx10] = '$65,000 to $74,999'
nhanesData$IncomeStr[ndx14] = '$75,000 to $99,999'
nhanesData$IncomeStr[ndx15] = '$100,000 and Over'

# Create Marital Status strings for plotting
nhanesData$MaritalStr = rep(1, nrow(nhanesData))
marriedNdx = nhanesData$DMDMARTL == 1
widowedNdx  = nhanesData$DMDMARTL == 2
divorcedNdx = nhanesData$DMDMARTL == 3
separatedNdx = nhanesData$DMDMARTL == 4
neverMarriedNdx = nhanesData$DMDMARTL == 5
partnerNdx = nhanesData$DMDMARTL == 6

nhanesData$MaritalStr[marriedNdx]        = 'Married'
nhanesData$MaritalStr[widowedNdx]       = 'Widowed'
nhanesData$MaritalStr[divorcedNdx]       = 'Divorced'
nhanesData$MaritalStr[separatedNdx]     = 'Separated'
nhanesData$MaritalStr[neverMarriedNdx] = 'Never Married'
nhanesData$MaritalStr[partnerNdx]         = 'Living with Partner'

# Create Race attribute with race strings for plotting
nhanesData$EdStr = rep(1, nrow(nhanesData))
ndx1 = nhanesData$DMDEDUC2 == 1
ndx2 = nhanesData$DMDEDUC2 == 2
ndx3 = nhanesData$DMDEDUC2 == 3
ndx4 = nhanesData$DMDEDUC2 == 4
ndx5 = nhanesData$DMDEDUC2 == 5
nhanesData$EdStr[ndx1] = 'Less Than 9th'
nhanesData$EdStr[ndx2] = '9th to 12th No Grad'
nhanesData$EdStr[ndx3] = 'High School Grad/GED'
nhanesData$EdStr[ndx4] = 'Some College/AA'
nhanesData$EdStr[ndx5] = 'College Grad/Above'

# Create hypertension attribute with strings for plotting
nhanesData$BPStr = rep(1, nrow(nhanesData))
ndx1 = nhanesData$BPQ020 == 1
ndx2 = nhanesData$BPQ020 == 2
nhanesData$BPStr[ndx1] = 'Hypertension'
nhanesData$BPStr[ndx2] = 'No Hypertension'

# Create diabetes attribute with strings for plotting
nhanesData$DiabetesStr = rep(1, nrow(nhanesData))
ndx1 = nhanesData$DIQ010 == 1
ndx2 = nhanesData$DIQ010 == 2
ndx3 = nhanesData$DIQ010 == 3
nhanesData$DiabetesStr[ndx1] = 'Diabetes'
nhanesData$DiabetesStr[ndx2] = 'No Diabetes'
nhanesData$DiabetesStr[ndx3] = 'Borderline Diabetes'

nhanesData$obeseType = rep(1, nrow(nhanesData))
nhanesData$obeseType <- ifelse(nhanesData$BMI_Perc < 30, "Not Obese", "Obese")

##################################################################################################################################
#
#  Write CSV file for other group members
#
##################################################################################################################################

write.csv(nhanesData, "C:\\DePaul Coursework\\Fall 2017 CSC 465\\Final Project\\NHANES_Filtered_ConbinedProjectDataset.csv")

##################################################################################################################################
#
#  Exploratory Histograms With Proper Data Labels
#
##################################################################################################################################

ggplot(data = nhanesData, aes(x = GenderStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Gender Counts") + 
  labs(x = "Gender", y = "Count")

ggplot(data = nhanesData, aes(x = RIDAGEYR)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Age Histogram") + 
  labs(x = "Age (Years)", y = "Frequency")

ggplot(data = nhanesData, aes(x = EdStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Education Counts") + 
  labs(x = "Education", y = "Count")

ggplot(data = nhanesData, aes(x = MaritalStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Marital Status Counts") + 
  labs(x = "Marital Status Levels", y = "Count")

ggplot(data = nhanesData, aes(x = RaceStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Race Counts") + 
  labs(x = "Races", y = "Count")

ggplot(data = nhanesData, aes(x = IncomeStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  scale_x_discrete(limits = c('$0 to $4,999', '$5,000 to $9,999', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999', 
                              '$25,000 to $34,999', '$35,000 to $44,999', '$45,000 to $54,999', '$55,000 to $64,999', 
                              '$65,000 to $74,999', '$75,000 to $99,999', '$100,000 and Over')) +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Income Counts") + 
  labs(x = "Income", y = "Count")

ggplot(data = nhanesData, aes(x = WHD010)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Height Histogram") + 
  labs(x = "Height (in)", y = "Frequency")

ggplot(data = nhanesData, aes(x = WHD020)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Weight Histogram") + 
  labs(x = "Weight (lb)", y = "Frequency")

ggplot(data = nhanesData, aes(x = obeseType)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Obesity Counts") + 
  labs(x = "Obese Levels", y = "Count")

ggplot(data = nhanesData, aes(x = BMI_Perc)) + geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("BMI Percentage Histogram") + 
  labs(x = "BMI Percentage", y = "Frequency")

ggplot(data = nhanesData, aes(x = BPStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Hypertension Counts") + 
  labs(x = "Hypertension Levels", y = "Count")

ggplot(data = nhanesData, aes(x = DiabetesStr)) + geom_bar(stat = "count", fill = "steelblue", color = "black") +
  theme( axis.text.x = element_text(angle = 20, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("Diabetes Counts") + 
  labs(x = "Diabetes Levels", y = "Count")

##################################################################################################################################
#
#  Creation of Heatmap with Age Versus Race and Color = BMI
#
##################################################################################################################################

ageMins = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75)
ageMaxs = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
nhanesData$ageGroups = rep(1, length(nhanesData$RIDAGEYR))
for (i in 1:length(ageMaxs)){
  ageStr = paste(ageMins[i], ' to ', ageMaxs[i])
  if (ageMaxs[i] == 80){
    ageNdx = (nhanesData$RIDAGEYR >= ageMins[i]) & (nhanesData$RIDAGEYR <= ageMaxs[i])
  }
  else{
    ageNdx = (nhanesData$RIDAGEYR >= ageMins[i]) & (nhanesData$RIDAGEYR < ageMaxs[i])
  } 
  nhanesData$ageGroups[ageNdx] = ageStr
}


aggData = as.data.frame(aggregate(nhanesData$BMI_Perc, list(nhanesData$ageGroups, nhanesData$RaceStr), mean))

ggplot(data = aggData, aes(x = Group.1, y = Group.2)) +
  geom_tile(aes(fill = aggData$x)) + 
  scale_fill_gradient(name = "Mean\nBMI", low = "yellow",high = "red") +
  theme( axis.text.x = element_text(angle = 90, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("BMI By Age and Race") + 
  labs(x = "Age (Years)", y = "Race")


aggData2 = as.data.frame(aggregate(nhanesData$BMI_Perc, list(nhanesData$ageGroups, nhanesData$GenderStr), mean))

ggplot(data = aggData2, aes(x = Group.1, y = Group.2)) +
  geom_tile(aes(fill = aggData2$x)) + 
  scale_fill_gradient(name = "Mean\nBMI", low = "yellow",high = "red") +
  theme( axis.text.x = element_text(angle = 90, hjust = 1), 
         panel.background = element_rect(fill = "white", color = "white",size = 0.5),
         panel.border = element_rect(colour = "black", fill=NA, size=1), 
         plot.title = element_text(hjust = 0.5), 
         legend.background = element_rect(color = "black",  fill = "white", size = 0.5, linetype = "solid")) +
  ggtitle("BMI By Age and Gender") + 
  labs(x = "Age (Years)", y = "Gender")



##################################################################################################################################
#
#  Creation of Stacked Bar Chart Of Count of Obese Versus Income (Income sorted by range values)
#
##################################################################################################################################

ggplot(nhanesData, aes(x=IncomeStr)) + 
  geom_bar(aes(fill=nhanesData$obeseType)) +
  scale_x_discrete(limits = c('$0 to $4,999', '$5,000 to $9,999', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999', 
                              '$25,000 to $34,999', '$35,000 to $44,999', '$45,000 to $54,999', '$55,000 to $64,999', 
                              '$65,000 to $74,999', '$75,000 to $99,999', '$100,000 and Over')) +
  scale_fill_manual(values = c("gray", "red")) +
  labs(x="Income", y="Count", title="Income Counts") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5))


##################################################################################################################################
#
#  Creation of Stacked Bar Chart Of Count of Obese Versus Marital Status
#
##################################################################################################################################

ggplot(nhanesData, aes(x=MaritalStr)) + 
  geom_bar(aes(fill=nhanesData$obeseType)) +
   scale_fill_manual(values = c("gray", "red")) +
  labs(x="Marital Status", y="Count", title="Marital Status Counts") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 20, hjust = 1))
  

##################################################################################################################################
#
#  Creation of Stacked Bar Chart Of Count of Obese Versus Education Level
#
##################################################################################################################################

ggplot(nhanesData, aes(x=EdStr)) + 
  geom_bar(aes(fill=nhanesData$obeseType)) +
  scale_fill_manual(values = c("gray", "red")) +
  labs(x="Education Level", y="Count", title="Education Level Counts") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 20, hjust = 1))


##################################################################################################################################
#
#  Creation of Stacked Bar Chart Of Count of Obese Versus Gender
#
##################################################################################################################################

ggplot(nhanesData, aes(x=GenderStr)) + 
  geom_bar(aes(fill=nhanesData$obeseType)) +
  scale_fill_manual(values = c("gray", "red")) +
  labs(x="Gender", y="Count", title="Gender Counts") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 20, hjust = 1))


##################################################################################################################################
#
#  Creation of Stacked Bar Chart Of Count of Obese Versus Race
#
##################################################################################################################################

ggplot(nhanesData, aes(x=RaceStr)) + 
  geom_bar(aes(fill=nhanesData$obeseType)) +
  scale_fill_manual(values = c("gray", "red")) +
  labs(x="Race", y="Count", title="Race Counts") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 20, hjust = 1))


##################################################################################################################################
#
#  Percent obese by income bar chart
#
##################################################################################################################################

# create data aggregated by year, with mean of adjusted close (before transformation)
uniqIncomes = unique(nhanesData$IncomeStr)

IncomeTotCount = rep(1, length(uniqIncomes))
ObeseIncCount = rep(1, length(uniqIncomes))
femaleObeseCount = rep(1, length(uniqIncomes))
maleObeseCount = rep(1, length(uniqIncomes))
femaleTotCount = rep(1, length(uniqIncomes))
maleTotCount = rep(1, length(uniqIncomes))
for (i in 1:length(uniqIncomes)){
  entryNdx = uniqIncomes[i]
  currentIncomeNdx = nhanesData$IncomeStr == entryNdx
  IncomeTotCount[i] = sum(currentIncomeNdx)
  
  currentIncomeData = nhanesData[currentIncomeNdx,]
  ObeseIncCount[i] = count(currentIncomeData$ObeseFlag)
  femaleTotCount[i] = sum(currentIncomeData$GenderStr == "Female")
  maleTotCount[i] = sum(currentIncomeData$GenderStr == "Male")
  
  obeseNdx = currentIncomeData$ObeseFlag == 1
  femaleObeseCount[i] = sum(currentIncomeData$GenderStr[obeseNdx] == "Female")
    maleObeseCount[i] = sum(currentIncomeData$GenderStr[obeseNdx] == "Male")
  
}

percObese = (ObeseIncCount / IncomeTotCount) * 100
femalePercObese = (femaleObeseCount / femaleTotCount) * 100
MalePercObese = (maleObeseCount / maleTotCount) * 100

aggData = data.frame(uniqIncomes, ObeseIncCount, IncomeTotCount, percObese, femalePercObese, MalePercObese)


femalePlot = ggplot(aggData, aes(x=uniqIncomes, femalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(limits = c('$0 to $4,999', '$5,000 to $9,999', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999', 
                              '$25,000 to $34,999', '$35,000 to $44,999', '$45,000 to $54,999', '$55,000 to $64,999', 
                              '$65,000 to $74,999', '$75,000 to $99,999', '$100,000 and Over')) +
  labs(x="Income", y="Percent Obese", title="Female Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0,12))

malePlot = ggplot(aggData, aes(x=uniqIncomes, MalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(limits = c('$0 to $4,999', '$5,000 to $9,999', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999', 
                              '$25,000 to $34,999', '$35,000 to $44,999', '$45,000 to $54,999', '$55,000 to $64,999', 
                              '$65,000 to $74,999', '$75,000 to $99,999', '$100,000 and Over')) +
  labs(x="Income", y="Percent Obese", title="Male Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0,12))


grid.arrange(malePlot, femalePlot, nrow=1)


##################################################################################################################################
#
#  Percent obese by education level bar chart
#
##################################################################################################################################

# create data aggregated by year, with mean of adjusted close (before transformation)
uniqEds = unique(nhanesData$EdStr)

EdTotCount = rep(1, length(uniqEds))
ObeseEdCount = rep(1, length(uniqEds))
femaleObeseCount = rep(1, length(uniqEds))
maleObeseCount = rep(1, length(uniqEds))
femaleTotCount = rep(1, length(uniqEds))
maleTotCount = rep(1, length(uniqEds))
for (i in 1:length(uniqEds)){
  entryNdx = uniqEds[i]
  currentEdNdx = nhanesData$EdStr == entryNdx
  EdTotCount[i] = sum(currentEdNdx)
  
  currentEdData = nhanesData[currentEdNdx,]
  ObeseEdCount[i] = count(currentEdData$ObeseFlag)
  femaleTotCount[i] = sum(currentEdData$GenderStr == "Female")
  maleTotCount[i] = sum(currentEdData$GenderStr == "Male")
  
  obeseNdx = currentEdData$ObeseFlag == 1
  femaleObeseCount[i] = sum(currentEdData$GenderStr[obeseNdx] == "Female")
  maleObeseCount[i] = sum(currentEdData$GenderStr[obeseNdx] == "Male")
  
}

percObese = (ObeseEdCount / EdTotCount) * 100
femalePercObese = (femaleObeseCount / femaleTotCount) * 100
MalePercObese = (maleObeseCount / maleTotCount) * 100

aggData = data.frame(uniqEds, ObeseEdCount, EdTotCount, percObese, femalePercObese, MalePercObese)


femalePlot = ggplot(aggData, aes(x=uniqEds, femalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Education Levels", y="Percent Obese", title="Female Percent Obese") +
  scale_x_discrete(limits = c('Less Than 9th', '9th to 12th No Grad', 'High School Grad/GED', 'Some College/AA', 'College Grad/Above')) +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,8))

malePlot = ggplot(aggData, aes(x=uniqEds, MalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Education Levels", y="Percent Obese", title="Male Percent Obese") +
  scale_x_discrete(limits = c('Less Than 9th', '9th to 12th No Grad', 'High School Grad/GED', 'Some College/AA', 'College Grad/Above')) +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,8))


grid.arrange(malePlot, femalePlot, nrow=1)


##################################################################################################################################
#
#  Percent obese by marital status bar chart
#
##################################################################################################################################

# create data aggregated by year, with mean of adjusted close (before transformation)
uniqMarital = unique(nhanesData$MaritalStr)

MaritalTotCount = rep(1, length(uniqMarital))
ObeseMaritalCount = rep(1, length(uniqMarital))
femaleObeseCount = rep(1, length(uniqMarital))
maleObeseCount = rep(1, length(uniqMarital))
femaleTotCount = rep(1, length(uniqMarital))
maleTotCount = rep(1, length(uniqMarital))
for (i in 1:length(uniqMarital)){
  entryNdx = uniqMarital[i]
  currentMaritalNdx = nhanesData$MaritalStr == entryNdx
  MaritalTotCount[i] = sum(currentMaritalNdx)
  
  currentMaritalData = nhanesData[currentMaritalNdx,]
  ObeseMaritalCount[i] = count(currentMaritalData$ObeseFlag)
  femaleTotCount[i] = sum(currentMaritalData$GenderStr == "Female")
  maleTotCount[i] = sum(currentMaritalData$GenderStr == "Male")
  
  obeseNdx = currentMaritalData$ObeseFlag == 1
  femaleObeseCount[i] = sum(currentMaritalData$GenderStr[obeseNdx] == "Female")
  maleObeseCount[i] = sum(currentMaritalData$GenderStr[obeseNdx] == "Male")
  
}

percObese = (ObeseMaritalCount / MaritalTotCount) * 100
femalePercObese = (femaleObeseCount / femaleTotCount) * 100
MalePercObese = (maleObeseCount / maleTotCount) * 100

aggData = data.frame(uniqMarital, ObeseMaritalCount, MaritalTotCount, percObese, femalePercObese, MalePercObese)


femalePlot = ggplot(aggData, aes(x=uniqMarital, femalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Marital Status", y="Percent Obese", title="Female Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0,12))

malePlot = ggplot(aggData, aes(x=uniqMarital, MalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Marital Status", y="Percent Obese", title="Male Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0,12))


grid.arrange(malePlot, femalePlot, nrow=1)


##################################################################################################################################
#
#  Percent obese by race bar chart
#
##################################################################################################################################

# create data aggregated by year, with mean of adjusted close (before transformation)
uniqRaces = unique(nhanesData$RaceStr)

RaceTotCount = rep(1, length(uniqRaces))
ObeseRaceCount = rep(1, length(uniqRaces))
femaleObeseCount = rep(1, length(uniqRaces))
maleObeseCount = rep(1, length(uniqRaces))
femaleTotCount = rep(1, length(uniqRaces))
maleTotCount = rep(1, length(uniqRaces))
for (i in 1:length(uniqRaces)){
  entryNdx = uniqRaces[i]
  currentRaceNdx = nhanesData$RaceStr == entryNdx
  RaceTotCount[i] = sum(currentRaceNdx)
  
  currentRaceData = nhanesData[currentRaceNdx,]
  ObeseRaceCount[i] = count(currentRaceData$ObeseFlag)
  femaleTotCount[i] = sum(currentRaceData$GenderStr == "Female")
  maleTotCount[i] = sum(currentRaceData$GenderStr == "Male")
  
  obeseNdx = currentRaceData$ObeseFlag == 1
  femaleObeseCount[i] = sum(currentRaceData$GenderStr[obeseNdx] == "Female")
  maleObeseCount[i] = sum(currentRaceData$GenderStr[obeseNdx] == "Male")
  
}

percObese = (ObeseRaceCount / RaceTotCount) * 100
femalePercObese = (femaleObeseCount / femaleTotCount) * 100
MalePercObese = (maleObeseCount / maleTotCount) * 100

aggData = data.frame(uniqRaces, ObeseRaceCount, RaceTotCount, percObese, femalePercObese, MalePercObese)


femalePlot = ggplot(aggData, aes(x=uniqRaces, femalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Race", y="Percent Obese", title="Female Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(0,10))

malePlot = ggplot(aggData, aes(x=uniqRaces, MalePercObese)) + geom_bar(stat = "identity", fill = "steelblue") +
  labs(x="Race", y="Percent Obese", title="Male Percent Obese") +
  theme(panel.background = element_rect(fill = "white", color = "white",size = 0.5, linetype = "solid"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(0,10))


grid.arrange(malePlot, femalePlot, nrow=1)


