###### Logistic Regression: Create the Cross-Validation-Function #######
library(tidyverse)
# Create a function which does the cross validation for the logistic regression------

# The inputs you gave the function
    # X_matrix = the matrix which includes the covariates and the target
    # fold = the k for the k-fold-CV
    # Target = here you have to indicate the name of the target-column
    # ColumnNumberTarget = here you have to indicate the column-number of the target
    # CriticalProbability = here you have to indicate the probability which separates a fire and a no-fire prediction

CrossValidationLogisticRegression=function(X_matrix, fold, Target, ColumnNumberTarget, CriticalProbability){
  # Prepare the cross-validation------
  k = fold
  ObservationPerFold = nrow(X_matrix)/k
  RoundedObservationPerFold = round(ObservationPerFold)
  NumberOfObservationsWeLookAt = k*RoundedObservationPerFold
  X_matrix=X_matrix[1:NumberOfObservationsWeLookAt,]
  
  # Build up some containers which you need within the for-loop
  beta_logistic = matrix(NA, nrow = k, ncol = ncol(X_matrix)) # Notice that you have add also a
  # column for the intercept
  # The eta matrix which includes the etas for all different folds
  eta_logistic_Training = matrix(NA, nrow = RoundedObservationPerFold*(k-1) , ncol = k) 
  eta_logistic_Testing = matrix(NA, nrow = RoundedObservationPerFold, ncol = k) 
  
  # The y-matrix which indicates whether there was really a fire or not
  Y_matrix_Training=matrix(NA, nrow = RoundedObservationPerFold*(k-1), ncol = k)
  Y_matrix_Testing=matrix(NA, nrow = RoundedObservationPerFold, ncol = k)
  
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
    # When it comes to keep all columns except one ( => '-'), R only understands the column 
    # when you indicate specific column number 
    
    # Run the algorithm and calculate the beta's
    data2 <- data.frame(X_matrix_Training) 
    model_logit = glm(Y_vector_Training ~., data2, family=binomial(link="logit")) 
    
    beta_logistic[(i+1),] = model_logit$coefficients # At the end, the model gives us the beta, what is exactly what
    # we are looking for when it comes to the logistic classifier (see slide 9)
    
    # Calculate the eta's for training and testing------
    for (j in 1:nrow(X_matrix_Training)) {
      eta_logistic_Training[j,(i+1)] = exp( c(1, X_matrix_Training[j,]) %*% beta_logistic[(i+1),])/(1 + exp(c(1, X_matrix_Training[j,]) %*% beta_logistic[(i+1),]))
      # The equation looks terrible, but basically it is just exactly the one we find yellow highlighted on
      # slide 9 of the classification slides: we take the Skalarpodukt oof x and beta. For the x-vector, we 
      # have to add a value=1 column in order that the Skalarprodukt works and the intercept is taken into account
      # We have to use a for-loop here in order to calculate the eta for every observations with its specific covariates
    }
    
    for (h in 1:nrow(X_matrix_Testing)) {
      eta_logistic_Testing[h,(i+1)] = exp( c(1, X_matrix_Testing[h,]) %*% beta_logistic[(i+1),])/(1 + exp(c(1, X_matrix_Testing[h,]) %*% beta_logistic[(i+1),]))
    }
    
  }
  
  # As a result from this for-loop, we get the following things:
  # A matrix with all different beta's
  # A matrix with all different training, and one with the Testing eta's
  
  # Do the predictions-----
  
  p = CriticalProbability # Define the critical probability which decides whether we predict a fire or not
  
  # Now we do the prediction for the Training
  PredictionTraining=matrix(NA, nrow = nrow(eta_logistic_Training), ncol = ncol(eta_logistic_Training))
  for(i in 1:ncol(eta_logistic_Training)){
    for(j in 1:nrow(eta_logistic_Training))
      PredictionTraining[j,i]=ifelse(eta_logistic_Training[j,i]>=p, 1, 0)
  }
  
  # And the prediction for the testing
  PredictionTesting=matrix(NA, nrow = nrow(eta_logistic_Testing), ncol = ncol(eta_logistic_Testing))
  for(i in 1:ncol(eta_logistic_Testing)){
    for(j in 1:nrow(eta_logistic_Testing))
      PredictionTesting[j,i]=ifelse(eta_logistic_Testing[j,i]>=p, 1, 0)
  }
  
  
  
  # Look at the training performance------
  
  # Create a matrix which show the type of errors, and another one which shows the type of rights
  Error_Matrix_Training=PredictionTraining-Y_matrix_Training # Where...
  # 1 = false positives
  # -1 = false negatives
  # 0 = right predictions
  
  Right_Matrix_Training=PredictionTraining+Y_matrix_Training # Where
  # 2 = right positives
  # 0 = right negatives
  # 1 = errors
  
  
  # Count the False Positives for the k trainings
  FalsePositivesPerFold_Training=matrix(NA, 1, ncol(Error_Matrix_Training))
  for(i in 1:ncol(Error_Matrix_Training)){
    FalsePositivesPerFold_Training[i]=length(which(Error_Matrix_Training[,i]==1))
  }
  
  # Count the False Negatives for the k trainings
  FalseNegativesPerFold_Training=matrix(NA, 1, ncol(Error_Matrix_Training))
  for(i in 1:ncol(Error_Matrix_Training)){
    FalseNegativesPerFold_Training[i]=length(which(Error_Matrix_Training[,i]==-1))
  }
  
  # Calculate the empirical error for the k trainings
  Empirical_error_Training=(FalsePositivesPerFold_Training+FalseNegativesPerFold_Training)/length(Y_vector_Training)
  
  # Calculate the Right Positives for the k trainings
  RightPositivesPerFold_Training=matrix(NA, 1, ncol(Right_Matrix_Training))
  for(i in 1:ncol(Right_Matrix_Training)){
    RightPositivesPerFold_Training[i]=length(which(Right_Matrix_Training[,i]==2))
  }
  
  # Calculate the Right Negatives for the k trainings
  RightNegativesPerFold_Training=matrix(NA, 1, ncol(Right_Matrix_Training))
  for(i in 1:ncol(Right_Matrix_Training)){
    RightNegativesPerFold_Training[i]=length(which(Right_Matrix_Training[,i]==0))
  }
  
  # Calculate the Percentage of right Classifications
  PercentageRightClassifications_Training=(RightPositivesPerFold_Training+RightNegativesPerFold_Training)/length(Y_vector_Training)
  
  # Calculate the Percentage of right predicted fires and no-fires
  PercentageRightClassifiedFires_Training=RightPositivesPerFold_Training/(RightPositivesPerFold_Training+FalseNegativesPerFold_Training)
  PercentageRightClassifiedNoFires_Training=RightNegativesPerFold_Training/(RightNegativesPerFold_Training+FalsePositivesPerFold_Training)
  
  # Calculate the mean empirical error within a specific setting (analog to the CVk for the testing)
  Mean_Empirical_error_Training=sum(Empirical_error_Training)/k
  
  # Calculate the mean percentage of right classified fires
  Mean_PercentageRightClassifiedFires_Training=sum(PercentageRightClassifiedFires_Training)/k
  
  # Calculate the mean percentage of right classified no-fires
  Mean_PercentageRightClassifiedNoFires_Training=sum(PercentageRightClassifiedNoFires_Training)/k
  
  # Look at the training performance
  FalsePositivesPerFold_Training
  FalseNegativesPerFold_Training
  Empirical_error_Training
  Mean_Empirical_error_Training
  
  RightPositivesPerFold_Training
  RightNegativesPerFold_Training
  PercentageRightClassifications_Training
  PercentageRightClassifiedFires_Training
  PercentageRightClassifiedNoFires_Training
  
  # Look at the testing performance------
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
  
  
  # Look at the Testing performance
  FalsePositivesPerFold_Testing
  FalseNegativesPerFold_Testing
  Empirical_error_Testing
  
  RightPositivesPerFold_Testing
  RightNegativesPerFold_Testing
  PercentageRightClassifications_Testing
  PercentageRightClassifiedFires_Testing
  PercentageRightClassifiedNoFires_Testing
  
  # Calculating the CVk
  CVk=sum(FalsePositivesPerFold_Testing+FalseNegativesPerFold_Testing)/nrow(X_matrix) # or
  CVk=sum(Empirical_error_Testing)/k
  CVk
  
  # Tell the function what it should return at the end-----
  Testing_Result=list("beta_logistic"=beta_logistic, "Empirical_error_Testing"=Empirical_error_Testing, 
                  "PercentageRightClassifiedFires_Testing"=PercentageRightClassifiedFires_Testing, 
                  "PercentageRightClassifiedNoFires_Testing"=PercentageRightClassifiedNoFires_Testing, 
                  "PercentageRightClassifications_Testing"=PercentageRightClassifications_Testing,
                  "CVk"=CVk)
  Training_Result=list("Empirical_error_Training"=Empirical_error_Training,
                       "PercentageRightClassifiedFires_Training"=PercentageRightClassifiedFires_Training, 
                       "PercentageRightClassifiedNoFires_Training"=PercentageRightClassifiedNoFires_Training,
                       "PercentageRightClassifications_Training"=PercentageRightClassifications_Training,
                       "Mean_Empirical_error_Training"=Mean_Empirical_error_Training,
                       "Mean_PercentageRightClassifiedFires_Training"=Mean_PercentageRightClassifiedFires_Training,
                       "Mean_PercentageRightClassifiedNoFires_Training"=Mean_PercentageRightClassifiedNoFires_Training)
  
  return(list("Testing_Result"=Testing_Result, "Training_Result"=Training_Result))
}




