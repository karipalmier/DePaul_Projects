##################################################################################################################################
#
#  Project Final.R
# 
#  This file contains the high level code used in creating the final models.  The first model created uses all the indep vars.
#  The second model removes influential points.  The third model removes extreme outliers.  Next models are made starting at the
#  backward and stepwise model selection recommendations with the regular Rent variable.  After that, the Box Cox of Rent is 
#  applied to the data after the extreme outliers were removed.  After that, models starting with the backward and stepwise
#  model selection recommendations with the Box Cox of the Rent variable.  During the creation of these models, first the coeffs
#  with the max p value above 0.05 are eliminated, then the coeffs with the max VIF over 10.  After the backward and stepwise 
#  models of both Rent and Box Cox Rent are generated, the data set is divided into training and testing sets and a new model is 
#  generated for each combo (Rent backward, Rent stepwise, Box Cox Rent backward, Box Cox Rent stepwise) using the training set.
#  Validatio is then done using the testing set.  Finally, new data points are imported and used to generate predictions for each
#  model combination.
#
#  Created By    Date
#  Kari Palmier  3/14/2017
#
##################################################################################################################################

library(psych)
library(QuantPsyc)
library(car)
library(leaps)
library(DAAG)

# Assign output file directory based on relative path of this R script
mainPath = getSrcDirectory(function(x) {x})
mainPath = gsub("/", "\\\\", mainPath)
mainPath = paste(mainPath, '\\', sep="")
dir.create(file.path(mainPath, "R Anlys Final"), showWarnings = FALSE)
anlysPath = paste(mainPath, "R Anlys Final\\", sep="")

# Get subfunctions required
source(paste(mainPath, "Common_R_Functions\\analyze_model.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\explore_data.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\select_model.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\validate_data.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\crossvalidate_data.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\get_outliers.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\model_data.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\remove_inf_pts.R", sep=""))
source(paste(mainPath, "Common_R_Functions\\predict_values.R", sep=""))

# Import Dataset
rentData = read.table(paste(mainPath, "\\Chicago_rent\\ChicagoProject.txt", sep=""), 
                      header=T)

###################################### Variable Initialization ###################################################################

varInfo.xVarNames = c("mdfamy0", "asianp", "whitenhp", "aanhp", "hisp", "renter", "educ8", 
                      "educ160pro", "Noheat", "totcrime", "murder", "robbery", "personal", "property", "forecl")
varInfo.xVarPlotNames = c("Median Family Income", "Prop of Asian American", 
                          "Prop of Non-Hispanic White", "Prop of Non-Hispanic African American", 
                          "Prop Hispanic American", "Prop of Renter Occupied", "Prop of With 8 Yrs School", 
                          "Prop With Bachelor or Grad School", "Prop With No Heat", "Prop Total Crime",
                          "Prop Murder", "Prop Robbery", "Prop Personal Crime", "Prop Property Crime", 
                          "Prop Foreclosures")
varInfo.yVarName = "Rent"
varInfo.yVarPlotName = "Median Gross Rent"

percTestPts = 25
percCrossValPts = 20
highRentLimit =  720
lowRentLimit = 450
bcHighRentLimit = 98
bcLowRentLimit = 73

outlierLevel = 5

###################################### Dataset Conversion ########################################################################

dir.create(file.path(anlysPath, "Init"), showWarnings = FALSE)
basePath = paste(anlysPath, "Init\\", sep="")

# Change the data frame columns from factor type to character type
i <- sapply(rentData, is.factor)
rentData[i] <- lapply(rentData[i], as.character)

# Remove any missing points from the dataset
outData = get_outliers(rentData, "-", "EQ", varInfo.xVarNames, basePath, "Explore_Init")

if(length(outData[,"Rent"]) != length(rentData[,"Rent"])){
  rentData = outData
}


# Change all data frame columns from character to numeric (except for the column of neighborhood names)
totVars = append(varInfo.yVarName, varInfo.xVarNames)
numVars = length(totVars)
for (i in 1:numVars){
  var = totVars[i]
  if (is.character(var)){
    rentData[var] = as.numeric(rentData[,var])
  }
}

################################################  Outlier Removal #################################################################

# Remove any 0 Rent entries since 0 rent doesn't make logical sense and can sway the data
outData = get_outliers(rentData, 0, "LTE", c("Rent"), basePath, "Explore_Init")

if(length(outData[,"Rent"]) != length(rentData[,"Rent"])){
  rentData = outData
}


################################################  Convert Foreclosures To Proportions ##############################################

rentData$forecl = rentData$forecl / 100

######################################## Neighborhood Dummy Variable Creation #####################################################

allVarInfo.xVarNames = varInfo.xVarNames
allVarInfo.xVarPlotNames = varInfo.xVarPlotNames

uniqAreas = unique(rentData$canam77)
numAreas = length(uniqAreas)
flagNames = c()
flagDescs = c()
for(i in 1:(numAreas-1)){
  val = uniqAreas[i]
  lowVal = tolower(val)
  matchNdx = which(rentData$canam77 %in% val)
  
  if(length(matchNdx) > 0){
    temp = gsub("-", "_", lowVal)
    temp2 = gsub("'", "", temp)
    newVal = paste(temp2, "Flag", sep="")
    
    tempFlag = (rentData$canam77 == val)*1
    rentData[newVal] = tempFlag
    
    flagNames = append(flagNames, newVal)
    flagDescs = append(flagDescs, val)
  }
}

numFlagVars = length(flagNames)
for(i in 1:numFlagVars){
  allVarInfo.xVarNames = append(allVarInfo.xVarNames, flagNames[i])
  allVarInfo.xVarPlotNames = append(allVarInfo.xVarPlotNames, flagDescs[i])
}

########################################## Assign x variables for functions ########################################################

varInfo.xVarNames = allVarInfo.xVarNames
varInfo.xVarPlotNames = allVarInfo.xVarPlotNames

########################################## Create Box Cox of Rent ###################################################################

# Compute Box Cox on Rent
numXVars = length(varInfo.xVarNames)
for(j in 1:numXVars){
  var = varInfo.xVarNames[j]
  
  if(j == 1){
    xformStr = var
  }else{
    xformStr = paste(xformStr, "+", var, sep="")
  }
}

formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
modelForm = as.formula(formStr)

transFit = powerTransform(modelForm, data=rentData, family="bcPower")
transInfo = summary(transFit)
yLambda = transInfo$result[1]

if(yLambda > 0.1){
  rentData$bcRent = ((rentData$Rent^yLambda) - 1)/yLambda
}else{
  rentData$bcRent = log(rentData$Rent)
}


######################################## Write dataset to a file for other group members ###########################################

write.table(rentData, paste(mainPath, "ChicagoProject_WithDummyNoBadPts.txt", sep=""), sep="\t", row.names=FALSE)

########################################## All Var Modelling ########################################################################

dir.create(file.path(anlysPath, "M1 All Vars"), showWarnings = FALSE)
basePath = paste(anlysPath, "M1 All Vars\\", sep="")

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = TRUE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE
baseData = data.frame()

varInfo.yVarName = "Rent"
varInfo.yVarPlotName = "Median Gross Rent"

modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")


########################################## Remove Influential Pts ###################################################################

dir.create(file.path(anlysPath, "M2 All Vars No Inf Pts"), showWarnings = FALSE)
basePath = paste(anlysPath, "M2 All Vars No Inf Pts\\", sep="")

infPtsNdx = modelOutParams[[1]]
if(length(infPtsNdx) > 0){
  
  outData = remove_inf_pts(rentData, infPtsNdx, basePath, "InfPts")
  rentData = outData
}

########################################## All Var Modelling w/o Inf Pts ############################################################

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = TRUE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")

########################################## Remove Extreme Outlier Pts ###############################################################

dir.create(file.path(anlysPath, "M3 All Vars No Out"), showWarnings = FALSE)
basePath = paste(anlysPath, "M3 All Vars No Out\\", sep="")

infPtsNdx = modelOutParams[[2]]
if(length(infPtsNdx) > 0){
  
  outData = remove_inf_pts(rentData, infPtsNdx, basePath, "ExtremeOut")
  rentData = outData
}


########################################## All Var Modelling w/o Outliers ##########################################################

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = TRUE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")

selNames = modelOutParams[[9]]
backTempNames = selNames[[1]]
stepTempNames = selNames[[2]]

########################################## Backward Sel Modelling ###################################################################

tempLength = length(backTempNames)
backVarInfo.xVarNames = backTempNames[2:tempLength]
numBackVars = length(backVarInfo.xVarNames)
tempPlotVars = c()
for(i in 1:numBackVars){
  var = backVarInfo.xVarNames[i]
  matchNdx = which(backVarInfo.xVarNames %in% var)
  tempPlotVars = append(tempPlotVars, varInfo.xVarPlotNames[matchNdx])
  
}

backVarInfo.xVarPlotNames = tempPlotVars  

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

dir.create(file.path(anlysPath, "M4 Back Init"), showWarnings = FALSE)
basePath = paste(anlysPath, "M4 Back Init\\", sep="")

varInfo.xVarNames = backVarInfo.xVarNames
varInfo.xVarPlotNames = backVarInfo.xVarPlotNames

maxPVal = 1
maxVIF = 20
modelNdx = 5
backCoeffsRmvd = c()
while((maxPVal > 0.05) | (maxVIF > 10)){
  
  modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")
  
  modelCoeffs = modelOutParams[[7]]
  coeffPVals = modelCoeffs[,4]
  maxPVal = max(coeffPVals)

  vifVals = data.frame(modelOutParams[[8]])
  maxVIF = max(vifVals)
  
  if(maxPVal > 0.05){
    maxNdx = which(coeffPVals %in% maxPVal)
    coeffNames = rownames(modelCoeffs)
    maxCoeffName = coeffNames[maxNdx]
    backCoeffsRmvd = append(backCoeffsRmvd, maxCoeffName)
    
    matchNdx = which(varInfo.xVarNames %in% maxCoeffName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxCoeffName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Back", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
  }else if (maxVIF > 10){
    tempVals = vifVals[,1]
    maxVifNdx = which(tempVals %in% maxVIF)
    vifNames = rownames(vifVals)
    maxVifName = vifNames[maxVifNdx]
    backCoeffsRmvd = append(backCoeffsRmvd, maxVifName)
    
    matchNdx = which(varInfo.xVarNames %in% maxVifName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxVifName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Back", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1

  }
}

finalBackVarInfo.xVarNames = varInfo.xVarNames
finalBackVarInfo.xVarPlotNames = varInfo.xVarPlotNames

########################################## Stepwise Sel Modelling ###################################################################

tempLength = length(stepTempNames)
stepVarInfo.xVarNames = stepTempNames[2:tempLength]
numStepVars = length(stepVarInfo.xVarNames)
tempPlotVars = c()
for(i in 1:numStepVars){
  var = stepVarInfo.xVarNames[i]
  matchNdx = which(stepVarInfo.xVarNames %in% var)
  tempPlotVars = append(tempPlotVars, varInfo.xVarPlotNames[matchNdx])
  
}

stepVarInfo.xVarPlotNames = tempPlotVars  

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

dir.create(file.path(anlysPath, "M4 Step Init"), showWarnings = FALSE)
basePath = paste(anlysPath, "M4 Step Init\\", sep="")

varInfo.xVarNames = stepVarInfo.xVarNames
varInfo.xVarPlotNames = stepVarInfo.xVarPlotNames

maxPVal = 1
modelNdx = 5
maxVIF = 20
stepCoeffsRmvd = c()
while((maxPVal > 0.05) | (maxVIF > 10)){
  
  modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")
  
  modelCoeffs = modelOutParams[[7]]
  coeffPVals = modelCoeffs[,4]
  maxPVal = max(coeffPVals)
  
  vifVals = data.frame(modelOutParams[[8]])
  maxVIF = max(vifVals)

    if(maxPVal > 0.05){
    maxNdx = which(coeffPVals %in% maxPVal)
    coeffNames = rownames(modelCoeffs)
    maxCoeffName = coeffNames[maxNdx]
    stepCoeffsRmvd = append(stepCoeffsRmvd, maxCoeffName)
    
    matchNdx = which(varInfo.xVarNames %in% maxCoeffName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxCoeffName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Step", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
    }else if (maxVIF > 10){
      tempVals = vifVals[,1]
      maxVifNdx = which(tempVals %in% maxVIF)
      vifNames = rownames(vifVals)
      maxVifName = vifNames[maxVifNdx]
      stepCoeffsRmvd = append(stepCoeffsRmvd, maxVifName)
      
      matchNdx = which(varInfo.xVarNames %in% maxVifName)
      varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxVifName]
      varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
      
      folderName = paste('M', modelNdx, " Step", sep="")
      dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
      basePath = paste(anlysPath, folderName, "\\", sep="")
      
      modelNdx = modelNdx + 1
      
    }
}

finalStepVarInfo.xVarNames = varInfo.xVarNames
finalStepVarInfo.xVarPlotNames = varInfo.xVarPlotNames

########################################## Box Cox All Var Modelling ################################################################

varInfo.xVarNames = allVarInfo.xVarNames
varInfo.xVarPlotNames = allVarInfo.xVarPlotNames

dir.create(file.path(anlysPath, "M4 All Vars BC"), showWarnings = FALSE)
basePath = paste(anlysPath, "M4 All Vars BC\\", sep="")

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = TRUE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

varInfo.yVarName = "bcRent"
varInfo.yVarPlotName = "BoxCox of Rent"

modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")

selNames = modelOutParams[[9]]
backTempNames = selNames[[1]]
stepTempNames = selNames[[2]]

########################################## Backward Sel Modelling ###################################################################

tempLength = length(backTempNames)
backVarInfo.xVarNames = backTempNames[2:tempLength]
numBackVars = length(backVarInfo.xVarNames)
tempPlotVars = c()
for(i in 1:numBackVars){
  var = backVarInfo.xVarNames[i]
  matchNdx = which(backVarInfo.xVarNames %in% var)
  tempPlotVars = append(tempPlotVars, varInfo.xVarPlotNames[matchNdx])
  
}

backVarInfo.xVarPlotNames = tempPlotVars  

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

dir.create(file.path(anlysPath, "M5 Back BC Init"), showWarnings = FALSE)
basePath = paste(anlysPath, "M5 Back BC Init\\", sep="")

varInfo.xVarNames = backVarInfo.xVarNames
varInfo.xVarPlotNames = backVarInfo.xVarPlotNames

maxPVal = 1
modelNdx = 6
maxVIF = 20
backCoeffsRmvdBc = c()
while((maxPVal > 0.05) | (maxVIF > 10)){
  
  modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")
  
  modelCoeffs = modelOutParams[[7]]
  coeffPVals = modelCoeffs[,4]
  maxPVal = max(coeffPVals)
  
  vifVals = data.frame(modelOutParams[[8]])
  maxVIF = max(vifVals)
  
  if(maxPVal > 0.05){
    maxNdx = which(coeffPVals %in% maxPVal)
    coeffNames = rownames(modelCoeffs)
    maxCoeffName = coeffNames[maxNdx]
    backCoeffsRmvdBc = append(backCoeffsRmvdBc, maxCoeffName)
    
    matchNdx = which(varInfo.xVarNames %in% maxCoeffName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxCoeffName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Back BC", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
  }else if (maxVIF > 10){
    tempVals = vifVals[,1]
    maxVifNdx = which(tempVals %in% maxVIF)
    vifNames = rownames(vifVals)
    maxVifName = vifNames[maxVifNdx]
    backCoeffsRmvdBc = append(backCoeffsRmvdBc, maxVifName)
    
    matchNdx = which(varInfo.xVarNames %in% maxVifName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxVifName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Back BC", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
    
  }
}

finalBackBcVarInfo.xVarNames = varInfo.xVarNames
finalBackBcVarInfo.xVarPlotNames = varInfo.xVarPlotNames

########################################## Stepwise Sel Modelling ###################################################################

tempLength = length(stepTempNames)
stepVarInfo.xVarNames = stepTempNames[2:tempLength]
numStepVars = length(stepVarInfo.xVarNames)
tempPlotVars = c()
for(i in 1:numStepVars){
  var = stepVarInfo.xVarNames[i]
  matchNdx = which(stepVarInfo.xVarNames %in% var)
  tempPlotVars = append(tempPlotVars, varInfo.xVarPlotNames[matchNdx])
  
}

stepVarInfo.xVarPlotNames = tempPlotVars  

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = FALSE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

dir.create(file.path(anlysPath, "M5 Step BC Init"), showWarnings = FALSE)
basePath = paste(anlysPath, "M5 Step BC Init\\", sep="")

varInfo.xVarNames = stepVarInfo.xVarNames
varInfo.xVarPlotNames = stepVarInfo.xVarPlotNames

maxPVal = 1
modelNdx = 6
maxVIF = 20
stepCoeffsRmvdBc = c()
while((maxPVal > 0.05) | (maxVIF > 10)){
  
  modelOutParams = model_data(rentData, baseData, modelFlags, 0, percCrossValPts, outlierLevel, varInfo, basePath, "AllData")
  
  modelCoeffs = modelOutParams[[7]]
  coeffPVals = modelCoeffs[,4]
  maxPVal = max(coeffPVals)
  
  vifVals = data.frame(modelOutParams[[8]])
  maxVIF = max(vifVals)
  
  if(maxPVal > 0.05){
    maxNdx = which(coeffPVals %in% maxPVal)
    coeffNames = rownames(modelCoeffs)
    maxCoeffName = coeffNames[maxNdx]
    stepCoeffsRmvdBc = append(stepCoeffsRmvdBc, maxCoeffName)
    
    matchNdx = which(varInfo.xVarNames %in% maxCoeffName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxCoeffName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Step BC", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
  }else if (maxVIF > 10){
    tempVals = vifVals[,1]
    maxVifNdx = which(tempVals %in% maxVIF)
    vifNames = rownames(vifVals)
    maxVifName = vifNames[maxVifNdx]
    stepCoeffsRmvdBc = append(stepCoeffsRmvdBc, maxVifName)
    
    matchNdx = which(varInfo.xVarNames %in% maxVifName)
    varInfo.xVarNames = varInfo.xVarNames[!varInfo.xVarNames %in% maxVifName]
    varInfo.xVarPlotNames = varInfo.xVarPlotNames[!varInfo.xVarPlotNames %in% varInfo.xVarPlotNames[matchNdx]]
    
    folderName = paste('M', modelNdx, " Step BC", sep="")
    dir.create(file.path(anlysPath, folderName), showWarnings = FALSE)
    basePath = paste(anlysPath, folderName, "\\", sep="")
    
    modelNdx = modelNdx + 1
    
  }
}

finalStepBcVarInfo.xVarNames = varInfo.xVarNames
finalStepBcVarInfo.xVarPlotNames = varInfo.xVarPlotNames

######################################## Create Training and Testing Datasets ######################################################

varInfo.yVarName = "Rent"
varInfo.yVarPlotName = "Median Gross Rent"

# Create sample indices
selectNdx = sample(1:nrow(rentData), (1-(percTestPts/100))*nrow(rentData))

# Select the training dataset
trainData = rentData[selectNdx, ]

# Select the testing dataset
testData = rentData[-selectNdx, ]

testSetDiff = setdiff(testData$canam77, trainData$canam77)
numDiff = length(testSetDiff)
numOrigRows = length(rentData$canam77)
allMatchNdx = c()
if(numDiff >0){
  for(i in 1:numDiff){
    val = testSetDiff[i]
    
    matchNdx = which(testData$canam77 %in% val)
    
    numMatches = length(matchNdx)
    if(matchNdx > 0){
      allMatchNdx = append(allMatchNdx, matchNdx)
    }
  }
  
  numAllMatches = length(allMatchNdx)
  percMatches = (numAllMatches / numOrigRows) * 100
  
  if(percMatches < 5){
    tempData = testData[allMatchNdx,]
    trainData = rbind(trainData, setNames(tempData, names(trainData)))    
    testData = testData[-allMatchNdx,]
  }
}


testSetDiff = setdiff(testData$canam77, trainData$canam77)
trainSetDiff = setdiff(trainData$canam77, testData$canam77)
trainDataPerc = (length(trainData$canam77)/numOrigRows)*100
testDataPerc = (length(testData$canam77)/numOrigRows)*100


outFileName = paste(anlysPath, "DataSet_Split_Information.txt", sep="")
outFile = file(outFileName, open="wt")

# Print cp model selection values to output file
sink(file=outFile, append=TRUE)
print("", quote=FALSE)
print("Neighborhoods in Testing data that are not in Training data:", quote=FALSE)
print("", quote=FALSE)
print(testSetDiff, quote=FALSE)
print("", quote=FALSE)
print("Neighborhoods in Training data that are not in Testing data:", quote=FALSE)
print("", quote=FALSE)
print(trainSetDiff, quote=FALSE)
print("", quote=FALSE)
print("", quote=FALSE)
print("Percentage of Original data used for Training data:", quote=FALSE)
print("", quote=FALSE)
print(round(trainDataPerc, digits=2), quote=FALSE)
print("", quote=FALSE)
print("", quote=FALSE)
print("Percentage of Original data used for Training data:", quote=FALSE)
print("", quote=FALSE)
print(round(testDataPerc, digits=2), quote=FALSE)
print("", quote=FALSE)
print("", quote=FALSE)
sink()

close(outFile)
closeAllConnections()


########################################## Backward Train/Test Modelling ############################################################

dir.create(file.path(anlysPath, "M Last Back"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Back\\", sep="")

varInfo.yVarName = "Rent"
varInfo.yVarPlotName = "Median Gross Rent"

varInfo.xVarNames = finalBackVarInfo.xVarNames
varInfo.xVarPlotNames = finalBackVarInfo.xVarPlotNames

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = TRUE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(trainData, testData, modelFlags, percTestPts, percCrossValPts, outlierLevel, varInfo, basePath, "TrainData")

########################################## Stepwise Train/Test Modelling ############################################################

dir.create(file.path(anlysPath, "M Last Step"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Step\\", sep="")

varInfo.xVarNames = finalStepVarInfo.xVarNames
varInfo.xVarPlotNames = finalStepVarInfo.xVarPlotNames

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = TRUE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(trainData, testData, modelFlags, percTestPts, percCrossValPts, outlierLevel, varInfo, basePath, "TrainData")

########################################## Backward BC Train/Test Modelling ##########################################################

dir.create(file.path(anlysPath, "M Last Back BC"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Back BC\\", sep="")

varInfo.yVarName = "bcRent"
varInfo.yVarPlotName = "BoxCox of Rent"

varInfo.xVarNames = finalBackBcVarInfo.xVarNames
varInfo.xVarPlotNames = finalBackBcVarInfo.xVarPlotNames

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = TRUE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(trainData, testData, modelFlags, percTestPts, percCrossValPts, outlierLevel, varInfo, basePath, "TrainData")

########################################## Stepwise BC Train/Test Modelling #########################################################

dir.create(file.path(anlysPath, "M Last Step BC"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Step BC\\", sep="")

varInfo.xVarNames = finalStepBcVarInfo.xVarNames
varInfo.xVarPlotNames = finalStepBcVarInfo.xVarPlotNames

# Perform all Y transforms with all x variables to see if any transforms improve modelling
modelFlags.performExp = TRUE
modelFlags.validateTest = TRUE
modelFlags.performModel = TRUE
modelFlags.makeExpPlots = FALSE
modelFlags.makeExpXXPlots = FALSE
modelFlags.quadXModel = FALSE

modelOutParams = model_data(trainData, testData, modelFlags, percTestPts, percCrossValPts, outlierLevel, varInfo, basePath, "TrainData")

########################################## Create Prediction Points #################################################################

predictData = read.table(paste(mainPath, "Rent_PredictionData.txt", sep=""), header=T)

# Change the data frame columns from factor type to character type
i <- sapply(predictData, is.factor)
predictData[i] <- lapply(predictData[i], as.character)

# Change all data frame columns from character to numeric (except for the column of neighborhood names)
totVars = length(colnames(predictData))
numVars = length(totVars)
for (i in 1:numVars){
  var = totVars[i]
  if (is.character(var)){
    predictData[var] = as.numeric(predictData[,var])
  }
}

uniqAreas = unique(rentData$canam77)
numAreas = length(uniqAreas)
for(i in 1:(numAreas-1)){
  val = uniqAreas[i]
  lowVal = tolower(val)
  
  temp = gsub("-", "_", lowVal)
  temp2 = gsub("'", "", temp)
  newVal = paste(temp2, "Flag", sep="")
  
  tempFlag = (predictData$canam77 == val)*1
  predictData[newVal] = tempFlag
}


predictData["Rent"] = NULL

########################################## Predict Points With Backward Model #######################################################

dir.create(file.path(anlysPath, "M Last Back"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Back\\", sep="")

varInfo.yVarName = "Rent"
varInfo.yVarPlotName = "Median Gross Rent"

varInfo.xVarNames = finalBackVarInfo.xVarNames
varInfo.xVarPlotNames = finalBackVarInfo.xVarPlotNames

numXVars = length(varInfo.xVarNames)
for(j in 1:numXVars){
  var = varInfo.xVarNames[j]
  
  if(j == 1){
    xformStr = var
  }else{
    xformStr = paste(xformStr, "+", var, sep="")
  }
}

formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
modelForm = as.formula(formStr)

backModel = lm(modelForm, data=rentData)

passFlag = predict_values(backModel, predictData, 0.95, yLambda, basePath, "AllData")

########################################## Predict Points With Stepwise Model #######################################################

dir.create(file.path(anlysPath, "M Last Step"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Step\\", sep="")

varInfo.xVarNames = finalStepVarInfo.xVarNames
varInfo.xVarPlotNames = finalStepVarInfo.xVarPlotNames

numXVars = length(varInfo.xVarNames)
for(j in 1:numXVars){
  var = varInfo.xVarNames[j]
  
  if(j == 1){
    xformStr = var
  }else{
    xformStr = paste(xformStr, "+", var, sep="")
  }
}

formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
modelForm = as.formula(formStr)

stepModel = lm(modelForm, data=rentData)

passFlag = predict_values(stepModel, predictData, 0.95, yLambda, basePath, "AllData")

########################################## Backward BC Train/Test Modelling ##########################################################

dir.create(file.path(anlysPath, "M Last Back BC"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Back BC\\", sep="")

varInfo.yVarName = "bcRent"
varInfo.yVarPlotName = "BoxCox of Rent"

varInfo.xVarNames = finalBackBcVarInfo.xVarNames
varInfo.xVarPlotNames = finalBackBcVarInfo.xVarPlotNames

numXVars = length(varInfo.xVarNames)
for(j in 1:numXVars){
  var = varInfo.xVarNames[j]
  
  if(j == 1){
    xformStr = var
  }else{
    xformStr = paste(xformStr, "+", var, sep="")
  }
}

formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
modelForm = as.formula(formStr)

backBcModel = lm(modelForm, data=rentData)

passFlag = predict_values(backBcModel, predictData, 0.95, yLambda, basePath, "AllData")

########################################## Stepwise BC Train/Test Modelling #########################################################

dir.create(file.path(anlysPath, "M Last Step BC"), showWarnings = FALSE)
basePath = paste(anlysPath, "M Last Step BC\\", sep="")

varInfo.xVarNames = finalStepBcVarInfo.xVarNames
varInfo.xVarPlotNames = finalStepBcVarInfo.xVarPlotNames

numXVars = length(varInfo.xVarNames)
for(j in 1:numXVars){
  var = varInfo.xVarNames[j]
  
  if(j == 1){
    xformStr = var
  }else{
    xformStr = paste(xformStr, "+", var, sep="")
  }
}

formStr = paste(varInfo.yVarName, "~", xformStr, sep="")
modelForm = as.formula(formStr)

stepBcModel = lm(modelForm, data=rentData)

passFlag = predict_values(stepBcModel, predictData, 0.95, yLambda, basePath, "AllData")

########################################## Create Output Model Sel Info File ########################################################

# Create output file
outFileName = paste(anlysPath, "Model_Sel_Info_R_Output.txt", sep="")
outFile = file(outFileName, open="wt")

# Print cp model selection values to output file
sink(file=outFile, append=TRUE)
print("", quote=FALSE)
print("Variables removed during backward selection of Rent (in order or removal):", quote=FALSE)
print("", quote=FALSE)
print(backCoeffsRmvd, quote=FALSE)
print("", quote=FALSE)
print("Variables removed during stepwise selection of Rent (in order or removal):", quote=FALSE)
print(stepCoeffsRmvd, quote=FALSE)
print("", quote=FALSE)
print("Variables removed during backward selection of Box Cox Rent (in order or removal):", quote=FALSE)
print("", quote=FALSE)
print(backCoeffsRmvdBc, quote=FALSE)
print("", quote=FALSE)
print("Variables removed during stepwise selection of Box Cox Rent (in order or removal):", quote=FALSE)
print(stepCoeffsRmvdBc, quote=FALSE)
print("", quote=FALSE)
sink()

close(outFile)
closeAllConnections()


