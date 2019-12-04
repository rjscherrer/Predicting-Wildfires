##### KNN: Final Test ######
source("Code/006_2_KNN_CV.R") # First you have to run this script which searches for the
# optimal number of NearestNeighbours and opitmal covariates for you

# Prepare the FinalTestSample------

# Choose only the covariates you determined as the best in the other script
FinalTestSample=select(FinalTestSample, c(Covariates_Lowest_CVk_Overall, "fire"))

# Omit the NA's and select the reality Y
FinalTestSample=na.omit(FinalTestSample)
Y_Reality=select(FinalTestSample, "fire")

# Get rid of the fire column for the FinalTestSample
FinalTestSample=select(FinalTestSample, -"fire")

# Get the training data and choose only the covariates you determined as the best------
Y_vector_Training=select(X_matrix, "fire")
Y_vector_Training=Y_vector_Training[,1]
X_matrix_Training=select(X_matrix, Covariates_Lowest_CVk_Overall)

# Do the predictions------
Prediction=knn(train = X_matrix_Training, test = FinalTestSample, Y_vector_Training, k=NumberOfNearestNeighbours_Lowest_CVk_Overall)

# Combine the prediction and the reality in order to prepare  the final error calculation
compare_FinalTest=cbind(Prediction, Y_Reality)

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


# Calculate the absolute errors and rights------

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





