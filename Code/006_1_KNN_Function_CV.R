###### KNN: Cross-Validation-Function #######
library("class")
library(tidyverse)
# The function--------

CrossValidationKNN=function(X_matrix, fold, Target, ColumnNumberTarget, NumberOfNearestNeighbours){
  # Prepare the cross-validation------
  k = fold
  ObservationPerFold = nrow(X_matrix)/k
  RoundedObservationPerFold = round(ObservationPerFold)
  NumberOfObservationsWeLookAt = k*RoundedObservationPerFold
  X_matrix=X_matrix[1:NumberOfObservationsWeLookAt,]
  
  # Build up some containers which you need within the for-loop

  
  # The y-matrix which indicates whether there was really a fire or not
  Y_matrix_Training=matrix(NA, nrow = RoundedObservationPerFold*(k-1), ncol = k)
  Y_matrix_Testing=matrix(NA, nrow = RoundedObservationPerFold, ncol = k)

  # The matrix which includes the predictions for all different folds
  PredictionTesting=matrix(NA, RoundedObservationPerFold, k)
  
  # Do the cross-validation-----
  for(i in 0:(k-1)){ # It has to be k-1 in order to get the last 1/10
    
    # Now, define the training and testing. The i makes sure that every fold is choosen as
    # testing once
    X_matrix_Training=X_matrix[-(((i*RoundedObservationPerFold+1)):((i+1)*RoundedObservationPerFold)),]
    X_matrix_Testing=X_matrix[(((i*RoundedObservationPerFold+1)):((i+1)*RoundedObservationPerFold)),]
    
    X_matrix_Training=as.matrix(X_matrix_Training)
    X_matrix_Testing=as.matrix(X_matrix_Testing)
    
    # Separate the Label-column
    Y_vector_Training=X_matrix_Training[, Target]
    Y_vector_Testing=X_matrix_Testing[, Target]
    
    # Fill up a Y_matrix. You will need them when it comes to estimating your performance
    Y_matrix_Training[,(i+1)]=Y_vector_Training
    Y_matrix_Testing[,(i+1)]=Y_vector_Testing
    
    # Remove the target - Column and set the data as matrizes since you need your covariates 
    # as a matrix in order to get the eta's at the end
    X_matrix_Training=X_matrix_Training[,-ColumnNumberTarget] 
    X_matrix_Testing=X_matrix_Testing[,-ColumnNumberTarget]
    # If you have to keep all columns except one ( => '-'), R only understands the column 
    # when you indicate specific column number 
    
    # Run the algorithm
    Predictions=knn(X_matrix_Training, X_matrix_Testing, Y_vector_Training, k = NumberOfNearestNeighbours, prob=FALSE)
    Predictions=as.numeric(as.vector(Predictions))
    PredictionTesting[,(i+1)]=Predictions
  }

  # Create a matrix which show the type of errors, and another one which shows the type of rights
  Error_Matrix_Testing=PredictionTesting-Y_matrix_Testing # Where...
  # 1 = false positives
  # -1 = false negatives
  # 0 = right predictions
  
  Right_Matrix_Testing=PredictionTesting+Y_matrix_Testing # Where
  # 2 = right positives
  # 0 = right negatives
  # 1 = errors

  # Count the False Positives for the k Testing
  FalsePositivesPerFold_Testing=matrix(NA, 1, ncol(Error_Matrix_Testing))
  for(i in 1:ncol(Error_Matrix_Testing)){
    FalsePositivesPerFold_Testing[i]=length(which(Error_Matrix_Testing[,i]==1))
  }
  
  # Count the False Negatives for the k Testing
  FalseNegativesPerFold_Testing=matrix(NA, 1, ncol(Error_Matrix_Testing))
  for(i in 1:ncol(Error_Matrix_Testing)){
    FalseNegativesPerFold_Testing[i]=length(which(Error_Matrix_Testing[,i]==-1))
  }
  
  # Calculate the empirical error for the k Testing
  Empirical_error_Testing=(FalsePositivesPerFold_Testing+FalseNegativesPerFold_Testing)/length(Y_vector_Testing)
  
  # Calculate the Right Positives for the k Testing
  RightPositivesPerFold_Testing=matrix(NA, 1, ncol(Right_Matrix_Testing))
  for(i in 1:ncol(Right_Matrix_Testing)){
    RightPositivesPerFold_Testing[i]=length(which(Right_Matrix_Testing[,i]==2))
  }
  
  # Calculate the Right Negatives for the k Testing
  RightNegativesPerFold_Testing=matrix(NA, 1, ncol(Right_Matrix_Testing))
  for(i in 1:ncol(Right_Matrix_Testing)){
    RightNegativesPerFold_Testing[i]=length(which(Right_Matrix_Testing[,i]==0))
  }
  
  # Calculate the Percentage of right Classifications
  PercentageRightClassifications_Testing=(RightPositivesPerFold_Testing+RightNegativesPerFold_Testing)/length(Y_vector_Testing)
  
  # Calculate the Percentage of right predicted fires and no-fires
  PercentageRightClassifiedFires_Testing=RightPositivesPerFold_Testing/(RightPositivesPerFold_Testing+FalseNegativesPerFold_Testing)
  PercentageRightClassifiedNoFires_Testing=RightNegativesPerFold_Testing/(RightNegativesPerFold_Testing+FalsePositivesPerFold_Testing)
  
  # Calculating the CVk
  CVk=sum(FalsePositivesPerFold_Testing+FalseNegativesPerFold_Testing)/nrow(X_matrix) # or
  CVk=sum(Empirical_error_Testing)/k
  CVk
  
  # Tell the function what it should return at the end-----
  CV_Results=list("Empirical_error_Testing"=Empirical_error_Testing, 
                  "PercentageRightClassifiedFires_Testing"=PercentageRightClassifiedFires_Testing, 
                  "PercentageRightClassifiedNoFires_Testing"=PercentageRightClassifiedNoFires_Testing, 
                  "PercentageRightClassifications_Testing"=PercentageRightClassifications_Testing,
                  "CVk"=CVk)
  
  return(CV_Results)
}
  






