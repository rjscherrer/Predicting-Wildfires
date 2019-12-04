##### LogisticRegression: Final Test ######
source("Code/007_2_LogisticRegression_CV.R") # First you have to run this script which searches for the
# optimal beta's and covariates for you

# Prepare the FinalTestSample------

# Choose only the covariates you determined as the best in the other script
FinalTestSample=select(FinalTestSample, c(Covariates_Lowest_CVk_Overall, "fire")) 

# Omit the NA's and select the reality Y
FinalTestSample=na.omit(FinalTestSample)
Y_Reality=select(FinalTestSample, "fire")

# Get rid of the fire column for the FinalTestSample
FinalTestSample=select(FinalTestSample, -"fire")

# Get the optimal beta's from the script------
Lowest_Empirical_error=min(ResultsForAllDifferentNumbersOfCovariates[[NumberOfCovariates_Lowest_CVk_Overall]]
    [[NumberOfSample_Lowest_CVk_Overall]][[1]][["Testing_Result"]][["Empirical_error_Testing"]])


RowOfOptimalBeta=which(ResultsForAllDifferentNumbersOfCovariates[[NumberOfCovariates_Lowest_CVk_Overall]]
      [[NumberOfSample_Lowest_CVk_Overall]][[1]][["Testing_Result"]][["Empirical_error_Testing"]]==Lowest_Empirical_error)



beta_matrix=ResultsForAllDifferentNumbersOfCovariates[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Testing_Result"]][["beta_logistic"]]

Optimal_beta=beta_matrix[RowOfOptimalBeta,]

# Do the predictions and get the eta's------

FinalTestSample=as.matrix(FinalTestSample)
eta_logistic_Testing=rep(NA, nrow(Y_Reality))

for(i in 1:nrow(Y_Reality)){
eta_logistic_Testing[i] = exp( c(1, FinalTestSample[i,]) %*% Optimal_beta)/(1 + exp(c(1, FinalTestSample[i,]) %*% Optimal_beta))
}

# Classify according to the predictions-------
p=CriticalProbabilityToTest
eta_logistic_Testing=as.matrix(eta_logistic_Testing)
Prediction=matrix(NA, nrow = nrow(eta_logistic_Testing), ncol = 1)
for(i in 1:nrow(eta_logistic_Testing)){
    Prediction[i]=ifelse(eta_logistic_Testing[i]>=p, 1, 0)
}

# Combine the prediction and the reality in order to prepare for the final error calculation
compare_FinalTest=cbind(eta_logistic_Testing, Prediction, Y_Reality)

# Create the results vectors-----

# Create a vector which provides you with the type of error
Prediction=as.double(as.matrix(Prediction))
Y_Reality=as.matrix(Y_Reality)


ErrorVector_FinalTest=Prediction-Y_Reality
# 1 = false positives
# -1 = false negatives
# 0 = right predictions

RightVector_FinalTest=Prediction+Y_Reality
# 2 = right positives
# 0 = right negatives
# 1 = errors

# Calculate the result determinants
FalsePositives_FinalTest=length(which(ErrorVector_FinalTest==1))

FalseNegatives_FinalTest=length(which(ErrorVector_FinalTest==-1))

RightPositives_FinalTest=length(which(RightVector_FinalTest==2))

RightNegatives_FinalTest=length(which(RightVector_FinalTest==0))


# Calculate the absolute errors and right------

# Create a vector which provides you with the type of error
compare_FinalTest[,1]-compare_FinalTest[,2]

# False Positives
class(compare_FinalTest)
length(which(compare_FinalTest[,1]>compare_FinalTest[,2]))

# Calculate the percentage error and rights
EmpiricalError_FinalTest=(FalsePositives_FinalTest+FalseNegatives_FinalTest)/nrow(FinalTestSample)
PercentageRightPositives_FinalTest=RightPositives_FinalTest/(RightPositives_FinalTest+FalseNegatives_FinalTest)
PercentageRightNegatives_FinalTest=RightNegatives_FinalTest/(RightNegatives_FinalTest+FalsePositives_FinalTest)
PercentageRightClassifications_FinalTest=(RightPositives_FinalTest+RightNegatives_FinalTest)/nrow(FinalTestSample)

EmpiricalError_FinalTest
PercentageRightPositives_FinalTest
PercentageRightNegatives_FinalTest
PercentageRightClassifications_FinalTest

Result_FinalTest=list("EmpiricalError_FinalTest"=EmpiricalError_FinalTest, 
                      "PercentageRightPositives_FinalTest"=PercentageRightPositives_FinalTest, 
                      "PercentageRightNegatives_FinalTest"=PercentageRightNegatives_FinalTest, 
                      "PercentageRightClassifications_FinalTest"=PercentageRightClassifications_FinalTest)

Result_FinalTest




