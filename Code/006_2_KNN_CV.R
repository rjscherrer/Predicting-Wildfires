###### KNN #######
rm(list = ls(all.names = TRUE))
library(tidyverse)
library("class")
source("Code/006_1_KNN_Function_CV.R")
load("Data/regressionData.RData")

# Get the data
X_matrix<-D

# Take out the FinalTestSample
SampledRows=sample(1:nrow(X_matrix), round(0.2*nrow(X_matrix)), replace=F)
FinalTestSample=X_matrix[SampledRows,]
X_matrix=X_matrix[-SampledRows,]

# Remove the NA-rows------
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


# Define your procedure-------
colnames(X_matrix)
# WhichCovariates= c("tmax", "rh", "wspd", "fire") 
WhichCovariates= c("tavg", "tmax", "tmin", "rh", "pressure", "wspd", "precip", 
                   "DSLR", "HPW1", "HPW2", "HPW3")

X_matrix = select(X_matrix, c(WhichCovariates, "fire")) # Add the target y

TargetExample="fire" # Indicate the target so you can easily drop and choose it within the for-loop
ColumnNumberTargetExample=which(colnames(X_matrix)==TargetExample) # Also indicate the column number of the
# target. For some reasons, sometimes you can drop a column only with its number

# Define how many random samples you want to generate------
ResultListForSpecificNumberOfCovariates=list()
ResultsForAllDifferentNumbersOfCovariates=list()
ResultsForSequenceOfNearestNeighboursToTest=list()
NumberOfCovariates=11
NumberOfRandomCombinations=1000
SequenceOfNearestNeighboursToTest= 1:10

# Generate the endlist-------
# Within the for-loop the endlist is called "ResultsForSequenceOfNearestNeighboursToTest"

# Make a for-loop which calculates the CVk for different levels of complexity and different covariates
# and different numbers of neares neighbours
for(h in SequenceOfNearestNeighboursToTest){
  for(i in 2:NumberOfCovariates){ # We have to start with 2 since my function does not work with
    # only 1 covariate
    for(j in 1:NumberOfRandomCombinations){
      RandomCovariates=sample(WhichCovariates, i, replace = F)
      X_matrix_specific = select(X_matrix, c(RandomCovariates, "fire"))
      TargetExample="fire"
      ColumnNumberTargetExample=which(colnames(X_matrix_specific)==TargetExample)
      Result=CrossValidationKNN(X_matrix = X_matrix_specific, fold = 6, Target = TargetExample,
                                ColumnNumberTarget = ColumnNumberTargetExample,
                                NumberOfNearestNeighbours = h)
      Result=list(Result, RandomCovariates) # Add to the result the covariates you used so you can look
      # them up afterwards
      ResultListForSpecificNumberOfCovariates[j]=list(Result) # A list, filled up with all the result for given covariates
    }
    ResultsForAllDifferentNumbersOfCovariates[i-1]=list(ResultListForSpecificNumberOfCovariates)
  }
  ResultsForSequenceOfNearestNeighboursToTest[h]=list(ResultsForAllDifferentNumbersOfCovariates)
}

# Take out all the CVk in order to compare them afterwards------
CVkForSequenceOfNearestNeighboursToTest=list()
CVkForSpecificNumberOfCovariates=list()
CVkForAllDifferentNumbersOfCovariates=list()
for(h in SequenceOfNearestNeighboursToTest){
  for(i in 2:NumberOfCovariates){ # Because we always choose min. 2 covariates
    for(j in 1:NumberOfRandomCombinations){
      CVkForSpecificNumberOfCovariates[j]=ResultsForSequenceOfNearestNeighboursToTest[[h]][[(i-1)]][[j]][[1]][["CVk"]]
    }
    CVkForAllDifferentNumbersOfCovariates[i-1]=list(CVkForSpecificNumberOfCovariates)
  }
  CVkForSequenceOfNearestNeighboursToTest[h]=list(CVkForAllDifferentNumbersOfCovariates)
}

# Overview of the result list:
    # First level: number of nearest neighbours
    # second level: number of covariates
    # third level: the samples

# Take the mean within a given number of covariates
CV_mean_ForAllDifferentNumberOfNearestNeighbours=matrix(NA,(NumberOfCovariates-1), length(SequenceOfNearestNeighboursToTest))
CVk_mean_ForAllDifferentNumbersOfCovariates=rep(NA, (NumberOfCovariates-1))

for(h in SequenceOfNearestNeighboursToTest){
  for(i in 2:NumberOfCovariates){
    CVk_mean_ForAllDifferentNumbersOfCovariates[i-1]=mean(as.numeric
                                                          (CVkForSequenceOfNearestNeighboursToTest[[h]][[i-1]]))
  }
  CV_mean_ForAllDifferentNumberOfNearestNeighbours[,h]=CVk_mean_ForAllDifferentNumbersOfCovariates
}
# The matrix includes 310columns for the 1-10 neighbours, and 10 rows, since we tried with 2 to 11 covariates
# = 10 different numbers of covariates

# Take out the lowest CVk
Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours=matrix(NA,(NumberOfCovariates-1), length(SequenceOfNearestNeighboursToTest))
Lowest_CVk_OfAllDifferentNumbersOfCovariates=rep(NA, (NumberOfCovariates-1)) # -1 because we 

for(h in SequenceOfNearestNeighboursToTest){
  for(i in 2:NumberOfCovariates){
    Lowest_CVk_OfAllDifferentNumbersOfCovariates[i-1]=min(as.numeric
                                                          (CVkForSequenceOfNearestNeighboursToTest[[h]][[i-1]]))
  }
  Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours[,h]=Lowest_CVk_OfAllDifferentNumbersOfCovariates
}

# Name the rows and the columns in order to get a better overview of the result-----

# Create the names for the columns
NN_AsColumnName=rep(NA, length(SequenceOfNearestNeighboursToTest))
for(h in SequenceOfNearestNeighboursToTest){
  NN_AsColumnName[[h]]=paste(print(h), "NearestNeighbours")
}

# Create the names for the rows
NumberOfCovariates_AsRowName=rep(NA, (NumberOfCovariates-1))
for(i in 2:NumberOfCovariates){
NumberOfCovariates_AsRowName[[i-1]]=paste(print(i), "Covariates")
}

# Name your 2 result types
colnames(CV_mean_ForAllDifferentNumberOfNearestNeighbours) <- NN_AsColumnName
rownames(CV_mean_ForAllDifferentNumberOfNearestNeighbours) <- NumberOfCovariates_AsRowName

colnames(Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours) <- NN_AsColumnName
rownames(Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours) <- NumberOfCovariates_AsRowName

# Look at the 2 result types
CV_mean_ForAllDifferentNumberOfNearestNeighbours
Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours

# Search the list with the lowest CVk-mean-------
Lowest_CV_mean_Overall=min(CV_mean_ForAllDifferentNumberOfNearestNeighbours)
Location_Lowest_CV_mean_overall=which(CV_mean_ForAllDifferentNumberOfNearestNeighbours==Lowest_CV_mean_Overall, arr.ind=T)
RowNumber_Lowest_CV_mean_overall=Location_Lowest_CV_mean_overall[1,1]
ColumnNumber_Lowest_CV_mean_overall=Location_Lowest_CV_mean_overall[1,2]

# Look up the list with the lowest CVk-mean 
ResultsForSequenceOfNearestNeighboursToTest[[ColumnNumber_Lowest_CV_mean_overall]][[RowNumber_Lowest_CV_mean_overall]]

# Search the exact place of the lowest CVk overall--------
Lowest_CV_Overall=min(Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours)
Location_Lowest_CV_Overall=which(Lowest_CVk_OfAllDifferentNumbersOfNearestNeighbours==Lowest_CV_Overall, arr.ind=T)
RowNumber_Lowest_CV_Overall=Location_Lowest_CV_Overall[1]
ColumnNumber_Lowest_CV_Overall=Location_Lowest_CV_Overall[2]

# Search the sample in which the lowest CVk overall happened
PossiblesSamplesWith_LowestCVK_Overall=rep(NA, NumberOfRandomCombinations)
for(i in 1:NumberOfRandomCombinations){
  PossiblesSamplesWith_LowestCVK_Overall[i]=ResultsForSequenceOfNearestNeighboursToTest[[ColumnNumber_Lowest_CV_Overall]][[RowNumber_Lowest_CV_Overall]][[i]][[1]][["CVk"]]
  
}
SamplesWith_LowestCVK_Overall=which(PossiblesSamplesWith_LowestCVK_Overall==Lowest_CV_Overall)

# Indicate the covariates which provided the lowest CVk overall
Covariates_Lowest_CVk_Overall=ResultsForSequenceOfNearestNeighboursToTest[[ColumnNumber_Lowest_CV_Overall]][[RowNumber_Lowest_CV_Overall]][[SamplesWith_LowestCVK_Overall]][[2]]
NumberOfNearestNeighbours_Lowest_CVk_Overall=ColumnNumber_Lowest_CV_Overall
NumberOfCovariates_Lowest_CVk_Overall=RowNumber_Lowest_CV_Overall+1 # +1 because we always have min 2 covariates
HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall=NumberOfCovariates_Lowest_CVk_Overall-1 # There you have to look on the second level
NumberOfSample_Lowest_CVk_Overall=SamplesWith_LowestCVK_Overall

Covariates_Lowest_CVk_Overall
NumberOfNearestNeighbours_Lowest_CVk_Overall
NumberOfCovariates_Lowest_CVk_Overall
HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall # Attention!!! Since we only tried with at least 2 covariates, 
# the first "folder" within the list stands for 2 covariates!
NumberOfSample_Lowest_CVk_Overall
Lowest_CV_Overall

# Finally, create two result list: one with the lowest mean CVk, and one with the lowest CVk overall-----

# Lowest mean CVk overall
    Lowest_CV_mean_Overall
    NumberOfNearestNeighbours=ColumnNumber_Lowest_CV_mean_overall
    NumverOfCovariates=RowNumber_Lowest_CV_mean_overall+1
    HowToSearchTheNumberOfCovariates_LowestMeanCVkOverall=RowNumber_Lowest_CV_mean_overall # Attention!!! Since we only tried with at least 2 covariates, the first "folder"
    # within the list stands for 2 covariates!
    
    Resultlist_Lowest_CVk_mean_Overall=list(Lowest_CV_mean_Overall, NumberOfNearestNeighbours, NumverOfCovariates, HowToSearchTheNumberOfCovariates_LowestMeanCVkOverall)
    Resultlist_Lowest_CVk_mean_Overall
    # Overview of the list
    # 1. The lowest CVk mean overall
    # 2. The number of nearest neighbours
    # 3. Te number of covariates
    # 4. How you have to search for the number of covariates, it is -1 since we never used only 1 covariate


# Lowest CVk overall
    Resultlist_Lowest_CVk_Overall=list("NumberOfNearestNeighbours_Lowest_CVk_Overall"=NumberOfNearestNeighbours_Lowest_CVk_Overall, 
                                       "NumberOfCovariates_Lowest_CVk_Overall"=NumberOfCovariates_Lowest_CVk_Overall, 
                                       "Covariates_Lowest_CVk_Overall"=Covariates_Lowest_CVk_Overall, 
                                       "HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall"=HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall, 
                                       "NumberOfSample_Lowest_CVk_Overall"=NumberOfSample_Lowest_CVk_Overall, 
                                       "Lowest_CV_Overall"=Lowest_CV_Overall)
    Resultlist_Lowest_CVk_Overall
    # Overview of the list
    # 1. The numver of nearest neighbours
    # 2. The number of covariates
    # 3. The name of the covariates
    # 4. How you have to search for the number of covariates, it is -1 since we never used only 1 covariate
    # 5. The number of the sample which brought us the lowest CVk overall => so you can find and target it within the endlist
    # 6. The lowest CVk overall

# Look at the whole process in the endlist----
    # You can find there the lowest CVK overall with the indicators above!
    Endlist=ResultsForSequenceOfNearestNeighboursToTest

