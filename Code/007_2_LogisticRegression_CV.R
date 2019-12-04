###### Logistic Regression with Cross Validation #######
rm(list = ls(all.names = TRUE))
library(tidyverse)
source("Code/007_1_LogisticRegression_Function_CV.R")
load("Data/regressionData.RData")

X_matrix<-D

# Take out the FinalTestSample
SampledRows=sample(1:nrow(X_matrix), round(0.2*nrow(X_matrix)), replace=F)
FinalTestSample=X_matrix[SampledRows,]
X_matrix=X_matrix[-SampledRows,]

# Remove the NA rows------
X_matrix<-na.omit(D) # remove NA-rows because the multiplication of matrizes does not work in this case

# Do some data cleaning------
# Take out some NoFires in order to get approximately 50% fire and no fire observations
NoFire=which(X_matrix$fire==0)
DropFires=sample(NoFire, 0.85*length(NoFire), replace = F)
X_matrix=X_matrix[-DropFires,] 

# Some values in the X_matrix are set as integer, change them all into numeric in order to make
# the algorithm work
for(i in 1:ncol(X_matrix)){
  X_matrix[,i]=as.numeric(X_matrix[,i])
}

class=rep(NA, ncol(X_matrix))
for(i in 1:ncol(X_matrix)){
  class[i]=class(X_matrix[,i])
}
class


# Choose your setting-------
colnames(X_matrix)
# WhichCovariates= c("tmax", "rh", "wspd", "fire") 
WhichCovariates= c("tavg", "tmax", "tmin", "rh", "pressure", "wspd", "precip", 
                   "DSLR", "HPW1", "HPW2", "HPW3") 

X_matrix = select(X_matrix, c(WhichCovariates, "fire"))

TargetExample="fire" # Indicate the target so you can easily drop and choose it within the for-loop
ColumnNumberTargetExample=which(colnames(X_matrix)==TargetExample) # Also indicate the column number of the
# target. For some reasons, sometimes you can drop a column only with its number


ResultListForSpecificNumberOfCovariates=list()
ResultsForAllDifferentNumbersOfCovariates=list()
NumberOfCovariates=11
NumberOfRandomCombinations=1000
CriticalProbabilityToTest=0.5

# Calculate the CVk for different levels of complexity and different covariates------

# Make a for-loop which calculates the CVk for different levels of complexity and different covariates
for(i in 2:NumberOfCovariates){ # We have to start with 2 since my function does not work with
  # only 1 covariate
  for(j in 1:NumberOfRandomCombinations){
    RandomCovariates=sample(WhichCovariates, i, replace = F)
    X_matrix_specific = select(X_matrix, c(RandomCovariates, "fire"))
    TargetExample="fire"
    ColumnNumberTargetExample=which(colnames(X_matrix_specific)==TargetExample)
    Result=CrossValidationLogisticRegression(X_matrix = X_matrix_specific, fold = 6, Target = TargetExample, 
                                           ColumnNumberTarget = ColumnNumberTargetExample, CriticalProbability = CriticalProbabilityToTest)
    Result=list(Result, RandomCovariates) # Add to the result the covariates you used so you can look
    # them up afterwards
    ResultListForSpecificNumberOfCovariates[j]=list(Result) # A list, filled up with all the result for given covariates
  }
  ResultsForAllDifferentNumbersOfCovariates[i]=list(ResultListForSpecificNumberOfCovariates)
}

# Take out all the CVk in order to compare them afterwards
CVkForSpecificNumberOfCovariates=list()
CVkForAllDifferentNumbersOfCovariates=list()
for(i in 2:NumberOfCovariates){
  for(j in 1:NumberOfRandomCombinations){
    CVkForSpecificNumberOfCovariates[j]=ResultsForAllDifferentNumbersOfCovariates[[i]][[j]][[1]][["Testing_Result"]][["CVk"]]
  }
  CVkForAllDifferentNumbersOfCovariates[i]=list(CVkForSpecificNumberOfCovariates)
}

# Take the mean within a given number of covariates
CVk_mean_ForAllDifferentNumbersOfCovariates=rep(NA, (NumberOfCovariates-1)) # -1 because we 
for(i in 2:NumberOfCovariates){
  CVk_mean_ForAllDifferentNumbersOfCovariates[i-1]=mean(as.numeric
                                                   (CVkForAllDifferentNumbersOfCovariates[[i]]))
}

# Take out the lowest numbers
Lowest_CVk_OfAllDifferentNumbersOfCovariates=rep(NA, (NumberOfCovariates-1)) # -1 because we 
for(i in 2:NumberOfCovariates){
  Lowest_CVk_OfAllDifferentNumbersOfCovariates[i-1]=min(as.numeric
                                                        (CVkForAllDifferentNumbersOfCovariates[[i]]))
}

# Name the rows and the columns in order to get a better overview of the result-----

# Create the names for the columns
NumberOfCovariates_AsColumnName=rep(NA, (NumberOfCovariates-1))
for(i in 2:NumberOfCovariates){
  NumberOfCovariates_AsColumnName[[i-1]]=paste(print(i), "Covariates")
}

names(CVk_mean_ForAllDifferentNumbersOfCovariates) <- NumberOfCovariates_AsColumnName
names(Lowest_CVk_OfAllDifferentNumbersOfCovariates) <- NumberOfCovariates_AsColumnName

# Look at the 2 result types
CVk_mean_ForAllDifferentNumbersOfCovariates
Lowest_CVk_OfAllDifferentNumbersOfCovariates

# Search the list with the lowest CVk-mean-------
Lowest_CVk_mean_Overall=min(CVk_mean_ForAllDifferentNumbersOfCovariates)
NumberOfCovariates_Lowest_CVk_mean_overall=(which(CVk_mean_ForAllDifferentNumbersOfCovariates==Lowest_CVk_mean_Overall, arr.ind=T)+1)

# Look up the list with the lowest CVk-mean 
ResultsForAllDifferentNumbersOfCovariates[[NumberOfCovariates_Lowest_CVk_mean_overall]] # +1 since for X covariates
# we get only 4 CVk_means since we never used only 1 covariate

# Search the exact place of the lowest CVk overall--------
Lowest_CV_Overall=min(Lowest_CVk_OfAllDifferentNumbersOfCovariates)
Location_Lowest_CV_Overall=min(which(Lowest_CVk_OfAllDifferentNumbersOfCovariates==Lowest_CV_Overall, arr.ind=T)+1) # +1 because
# we did never look at only one covariate

# Search the sample in which the lowest CVk overall happened
PossiblesSamplesWith_LowestCVK_Overall=rep(NA, NumberOfRandomCombinations)
for(i in 1:NumberOfRandomCombinations){
  PossiblesSamplesWith_LowestCVK_Overall[i]=ResultsForAllDifferentNumbersOfCovariates[[Location_Lowest_CV_Overall]][[i]][[1]][["Testing_Result"]][["CVk"]]
  
}
SamplesWith_LowestCVK_Overall=min(which(PossiblesSamplesWith_LowestCVK_Overall==Lowest_CV_Overall))

# Indicate the covariates which provided the lowest CVk overall
Covariates_Lowest_CVk_Overall=ResultsForAllDifferentNumbersOfCovariates[[Location_Lowest_CV_Overall]][[SamplesWith_LowestCVK_Overall]][[2]]
NumberOfCovariates_Lowest_CVk_Overall=Location_Lowest_CV_Overall
NumberOfSample_Lowest_CVk_Overall=SamplesWith_LowestCVK_Overall

# And, finally create two result list: one with the lowest mean CVk, and one with the lowest CVk overall-----

# Lowest mean CVk overall
    Resultlist_Lowest_CVk_mean_Overall=list(Lowest_CVk_mean_Overall, NumberOfCovariates_Lowest_CVk_mean_overall)
    Resultlist_Lowest_CVk_mean_Overall
    # Overview of the list
    # 1. The lowest CVk mean overall
    # 2. The number of Covariates which brought us this mean
    
    # ATTENTION: There are no covariates indicated here, since we have several combinations/samples within a given
    # number of covariates

# Lowest CVk overall
    Resultlist_Lowest_CVk_Overall=list("Covariates_Lowest_CVk_Overall"=Covariates_Lowest_CVk_Overall, 
                                       "NumberOfCovariates_Lowest_CVk_Overall"=NumberOfCovariates_Lowest_CVk_Overall, 
                                       "NumberOfSample_Lowest_CVk_Overall"=NumberOfSample_Lowest_CVk_Overall,
                                       "Lowest_CV_Overall"=Lowest_CV_Overall)
    Resultlist_Lowest_CVk_Overall
    
    # Overview of the list
    # 1. The covariates which brought us the lowest CVk overall
    # 2. The number of covariates
    # 3. The number of the sample which brought us the lowest CVk overall
    # 4. The lowest CVk overall

# Look at the whole process in the end list----
    # You can find there the lowest CVK overall with the indicators above!
    Endlist=ResultsForAllDifferentNumbersOfCovariates




