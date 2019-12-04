rm(list = ls())

library(tidyverse)
library(neuralnet)

load("Data/regressionData.RData")

X_matrix<-D

#using the best covariates from the original neural network file
covariates <- c("wspd", "tmax", "rh", "tavg", "weather_11", "weather_21", "weather_26", 
                "weather_28", "weather_29", "weather_30", "weather_33", "fire")


# Take out the FinalTestSample
SampledRows=sample(1:nrow(X_matrix), round(0.2*nrow(X_matrix)), replace=F)

FinalTestSample=X_matrix[SampledRows,]
FinalTestSample <- select(FinalTestSample, covariates)
FinalTestSample <- na.omit(FinalTestSample)
############################3

load("Data/regressionData.RData")
D <- na.omit(D)
D_no_fire <- which(D$fire == 0)
D <- D[-sample(D_no_fire, 0.85*length(D_no_fire), replace = F),] 

set.seed(123)
rows <- sample(nrow(D))
D <- D[rows, ]

library(rsample)
split <- initial_split(D, prop = .7)

training_data <- training(split)
y_train <- training_data$fire
training_data <- training_data[ ,!(colnames(training_data) == "fire" | colnames(training_data) == "date")]

testing_data  <- testing(split)
y_test <- testing_data$fire
testing_data <- testing_data[ ,!(colnames(testing_data) == "fire" | colnames(testing_data) == "date")]

########################################################
##
## Cross validation which columns perform the best
##
########################################################
colnames <- colnames(D[, !(colnames(D) %in% c("date","fire", "wspd", "tmax", "rh")), drop=FALSE])
col_results <- data.frame(matrix(NA, nrow = 10, ncol = 22))
names(col_results) <- colnames

for(x in 1:length(colnames)) {
  pred_results <- c()
  colname <- colnames[x]
  for(z in 1:10) {
    selection <- paste("y_train ~ wspd + tmax + rh", colname, sep = " + ")
    nn = neuralnet(selection, data = training_data, hidden = 2, act.fct = "tanh", linear.output = FALSE, stepmax=1e6)
    # plot(nn)
    
    predict = neuralnet::compute(nn, testing_data)
    # predict$net.result
    prob <- predict$net.result
    pred <- ifelse(prob>0.5, 1, 0)
    results <- data.frame(y_test, pred)
    
    n_error = 0
    false_pos = 0
    false_neg = 0
    correct_pos = 0
    correct_neg = 0
    for(i in 1:nrow(results)) {
      if (results[i,]$y_test != results[i,]$pred) {
        n_error = n_error + 1
        if(results[i,]$y_test == 1) {
          false_neg = false_neg + 1
        } else {
          false_pos = false_pos + 1
        }
      } else {
        if(results[i,]$y_test == 1) {
          correct_pos = correct_pos + 1
        } else {
          correct_neg = correct_neg + 1
        }
      }
    }
    
    emp_error = n_error / nrow(results) * 100
    pred_results[z] <- emp_error
    message("Spalte: ", toString(x), " Zeile: ", toString(z))
  }
  pred_results
  col_results[,x] <- pred_results
}

means <- apply(col_results, 2, mean)
better_columns <- means[means < 39.53545]




##########
########################################################
##
## Cross validation
##
########################################################
require(neuralnet)
library(rsample)
library(tidyverse)

load("Data/regressionData.RData")
D <- na.omit(D)
D_no_fire <- which(D$fire == 0)
D <- D[-sample(D_no_fire, 0.85*length(D_no_fire), replace = F),]

set.seed(123)
rows <- sample(nrow(D))
D <- D[rows, ]

# selection <- "y_train ~ wspd + tmax + rh + tavg + weather_11 + weather_21 + weather_26 + weather_28 + weather_29 + weather_30 + weather_33"
covariates <- c("wspd", "tmax", "rh", "tavg", "weather_11", "weather_21", "weather_26",
                "weather_28", "weather_29", "weather_30", "weather_33", "fire")
D <- select(D, covariates)
target <- "fire"
column_number_target <- which(colnames(D) == target)

k <- 10
observation_per_fold <- nrow(D)/k
rounded_observation_per_fold <- round(observation_per_fold)
number_of_observations_we_look_at <- k*rounded_observation_per_fold
D=D[1:number_of_observations_we_look_at,]

weights <- list()

eta_neural_training <- matrix(NA, nrow = rounded_observation_per_fold*(k-1) , ncol = k)
eta_neural_testing <- matrix(NA, nrow = rounded_observation_per_fold, ncol = k)

y_matrix_training <- matrix(NA, nrow = rounded_observation_per_fold*(k-1), ncol = k)
y_matrix_testing <- matrix(NA, nrow = rounded_observation_per_fold, ncol = k)

prediction_training <- matrix(NA, nrow = nrow(eta_neural_training), ncol = ncol(eta_neural_training))
prediction_testing <- matrix(NA, nrow = nrow(eta_neural_testing), ncol = ncol(eta_neural_testing))
j <- k-1

weights <- list()
nn_list <- list()

for(i in 0:j) {
  x_matrix_training <- D[-(((1*rounded_observation_per_fold+1)):((1+1)*rounded_observation_per_fold)),]
  x_matrix_testing <- D[(((1*rounded_observation_per_fold+1)):((1+1)*rounded_observation_per_fold)),]
  
  x_matrix_training <- as.matrix(x_matrix_training)
  x_matrix_testing <- as.matrix(x_matrix_testing)
  
  y_vector_training <- x_matrix_training[, target]
  y_vector_testing <- x_matrix_testing[, target]
  
  y_matrix_training[,(i+1)] <- y_vector_training
  y_matrix_testing[,(i+1)] <- y_vector_testing
  
  x_matrix_training <- x_matrix_training[,-column_number_target]
  x_matrix_testing <- x_matrix_testing[,-column_number_target]
  
  data <- data.frame(y_vector_training, x_matrix_training)
  data2 <- data.frame(x_matrix_training)
  
  nn = neuralnet(y_vector_training ~., data = data2, hidden = 2, act.fct = "tanh", linear.output = FALSE, stepmax=1e6)
  nn_list[[i+1]] <- nn
  weights[[i+1]] <- nn[["weights"]][[1]]
  eta_neural_training[,(i+1)] <- nn$net.result[[1]]
  prediction_training[,(i+1)] <- ifelse(nn$net.result[[1]]>0.5, 1, 0)
  
  predict = predict(nn, x_matrix_testing)
  eta_neural_testing[,(i+1)] <- predict[,1]
  prediction_testing[,(i+1)] <- ifelse(predict[,1]>0.5, 1, 0)
}

error_matrix_training <- prediction_training-y_matrix_training
right_matrix_training <- prediction_training-y_matrix_training

FalsePositivesPerFold_Training=matrix(NA, 1, ncol(error_matrix_training))
for(i in 1:ncol(error_matrix_training)){
  FalsePositivesPerFold_Training[i]=length(which(error_matrix_training[,i]==1))
}

FalseNegativesPerFold_Training=matrix(NA, 1, ncol(error_matrix_training))
for(i in 1:ncol(error_matrix_training)){
  FalseNegativesPerFold_Training[i]=length(which(error_matrix_training[,i]==-1))
}

Empirical_error_Training=(FalsePositivesPerFold_Training+FalseNegativesPerFold_Training)/length(y_vector_training)

RightPositivesPerFold_Training=matrix(NA, 1, ncol(right_matrix_training))
for(i in 1:ncol(right_matrix_training)){
  RightPositivesPerFold_Training[i]=length(which(right_matrix_training[,i]==2))
}

RightNegativesPerFold_Training=matrix(NA, 1, ncol(right_matrix_training))
for(i in 1:ncol(right_matrix_training)){
  RightNegativesPerFold_Training[i]=length(which(right_matrix_training[,i]==0))
}

PercentageRightClassifications_Training=(RightPositivesPerFold_Training+RightNegativesPerFold_Training)/length(y_vector_training)

FalsePositivesPerFold_Training
FalseNegativesPerFold_Training
Empirical_error_Training

RightPositivesPerFold_Training
RightNegativesPerFold_Training
PercentageRightClassifications_Training


Error_Matrix_Testing=prediction_testing-y_matrix_testing
Right_Matrix_Testing=prediction_testing+y_matrix_testing

FalsePositivesPerFold_Testing=matrix(NA, 1, ncol(Error_Matrix_Testing))
for(i in 1:ncol(Error_Matrix_Testing)){
  FalsePositivesPerFold_Testing[i]=length(which(Error_Matrix_Testing[,i]==1))
}

FalseNegativesPerFold_Testing=matrix(NA, 1, ncol(Error_Matrix_Testing))
for(i in 1:ncol(Error_Matrix_Testing)){
  FalseNegativesPerFold_Testing[i]=length(which(Error_Matrix_Testing[,i]==-1))
}

Empirical_error_Testing=(FalsePositivesPerFold_Testing+FalseNegativesPerFold_Testing)/length(y_vector_testing)

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
PercentageRightClassifications_Testing=(RightPositivesPerFold_Testing+RightNegativesPerFold_Testing)/length(y_vector_testing)


# Look at the Testing performance
FalsePositivesPerFold_Testing
FalseNegativesPerFold_Testing
Empirical_error_Testing

RightPositivesPerFold_Testing
RightNegativesPerFold_Testing
PercentageRightClassifications_Testing

# Calculating the CVk

CVk=sum(FalsePositivesPerFold_Testing+FalseNegativesPerFold_Testing)/nrow(D) # or
CVk=sum(Empirical_error_Testing)/k
CVk

best_weights <- weights[which.min(Empirical_error_Testing)]
best_index <- which.min(Empirical_error_Testing)

final_x <- as.matrix(select(FinalTestSample, -fire))
final_y <- FinalTestSample$fire
final_nn <- nn_list[best_index]
predict = neuralnet::compute(nn_list[[best_index]], final_x)
final_predict <- ifelse(predict$net.result > 0.5, 1, 0)

final_falsepos <- ifelse(final_predict == 1 & final_y == 0, 1, 0)
final_falsepos <- sum(final_falsepos)
final_falseneg <- ifelse(final_predict == 0 & final_y == 1, 1, 0)
final_falseneg <- sum(final_falseneg)

final_corpos <- ifelse(final_predict == 1 & final_y == 1, 1, 0)
final_corpos <- sum(final_corpos)

final_corneg <- ifelse(final_predict == 0 & final_y == 0, 1, 0)
final_corneg <- sum(final_corneg)

final_emperror <- (final_falseneg + final_falsepos) / length(final_y)

final_data <- data.frame(final_falsepos, final_falseneg, final_corpos, final_corneg)
colnames(final_data) <- c("falsePos", "falseNeg", "correctPos", "correctNeg")

save(best_weights, file = "Data/best_weights.RData")
save(final_data, file = "Data/final_stats.RData")

# print(paste("Empirical Error: ", toString(round(emp_error, digits = 2)), "%", sep = ""))
# print(paste("False Positive: ", toString(false_pos), sep = ""))
# print(paste("False Negative: ", toString(false_neg), sep = ""))
# print(paste("Correct Positive: ", toString(correct_pos), sep = ""))
# print(paste("Correct Negative: ", toString(correct_neg), sep = ""))


