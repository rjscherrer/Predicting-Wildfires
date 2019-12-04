##### Logistic Regression: Presentation Results #######
source("Code/007_3_LogisticRegression_FinalTest.R")

# Matrix which shows the percentage of Right Positives/Negatives and false Positives/Negatives-------
SumOf_Fires=sum(RightPositives_FinalTest, FalseNegatives_FinalTest)
SumOf_NoFires=sum(RightNegatives_FinalTest, FalsePositives_FinalTest)
SumOf_Observations=sum(SumOf_Fires, SumOf_NoFires)
FireColumn=rbind(RightPositives_FinalTest/SumOf_Observations, FalseNegatives_FinalTest/SumOf_Observations,
            sum(RightPositives_FinalTest/SumOf_Observations, FalseNegatives_FinalTest/SumOf_Observations))
NoFireColumn=rbind(FalsePositives_FinalTest/SumOf_Observations, RightNegatives_FinalTest/SumOf_Observations,
              sum(FalsePositives_FinalTest/SumOf_Observations, RightNegatives_FinalTest/SumOf_Observations))
RightFalse_PositivesNegatives_matrix=cbind(FireColumn, NoFireColumn)
RightFalse_PositivesNegatives_matrix=RightFalse_PositivesNegatives_matrix

rownames(RightFalse_PositivesNegatives_matrix) <- rbind("Prediction: Fire", "Prediction: No Fire", "Total")
colnames(RightFalse_PositivesNegatives_matrix) <- cbind("Fire", "No Fire")


# Matrix which shows the percentage of right predicted fires and right predicted no fires------
Fires=rbind(Result_FinalTest$PercentageRightPositives_FinalTest, 1-Result_FinalTest$PercentageRightPositives_FinalTest)
NoFires=rbind(1-Result_FinalTest$PercentageRightNegatives_FinalTest, Result_FinalTest$PercentageRightNegatives_FinalTest)
RightPercentage_matrix=cbind(Fires, NoFires)

rownames(RightPercentage_matrix) <- rbind("Prediction: Fire", "Prediction: No Fire")
colnames(RightPercentage_matrix) <- cbind("Fire", "No Fire")


# Get an overview of the result of the final test--------
PresentationList=list("EmpiricalError_FinalTest"=EmpiricalError_FinalTest, 
                      "RightFalse_PositivesNegatives_matrix"=RightFalse_PositivesNegatives_matrix, 
                      "RightPercentage_matrix"=RightPercentage_matrix, 
                      "NumberOfCovariates_Lowest_CVk_Overall"=NumberOfCovariates_Lowest_CVk_Overall, 
                      "Covariates_Lowest_CVk_Overall"=Covariates_Lowest_CVk_Overall,
                      "Optimal_beta"=Optimal_beta,
                      "NumberOfSample_Lowest_CVk_Overall"=NumberOfSample_Lowest_CVk_Overall,
                      "CVk_mean_ForAllDifferentNumbersOfCovariates"=CVk_mean_ForAllDifferentNumbersOfCovariates)

# Look at the optimal sample
Sample_Lowest_CVk_Overall=Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]]


# barplot the Right-Percentage-matrix------
RighPredictions=100*cbind(Result_FinalTest$PercentageRightPositives_FinalTest, Result_FinalTest$PercentageRightNegatives_FinalTest, 1-Result_FinalTest$EmpiricalError_FinalTest)
FalsePredictions=100*cbind(1-Result_FinalTest$PercentageRightPositives_FinalTest, 1-Result_FinalTest$PercentageRightNegatives_FinalTest, Result_FinalTest$EmpiricalError_FinalTest)
barplot_RightPercentage_matrix=rbind(RighPredictions, FalsePredictions)
rownames(barplot_RightPercentage_matrix) <- rbind("Right", "False")
colnames(barplot_RightPercentage_matrix) <- cbind(paste(SumOf_Fires, "Fires"), paste(SumOf_NoFires, "No Fires"), paste(SumOf_Observations, "Days"))
par(mar=c(5, 5, 5, 6)) # bottom, left, top, right
barplot(barplot_RightPercentage_matrix, main="Final Test Result",
        col=c("darkgreen","red"),legend = rownames(barplot_RightPercentage_matrix), ylab = "in %",
        args.legend = list(x = "right", title = "Prediction", bty = "n", inset=c(-0.3, -0.3)), border = TRUE)
abline(h=50, col = "black", lwd= 3, lty = 3)

    # Do it also for the training (you need it for the documentation)
    RightPredictions_Training=100*cbind(Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_PercentageRightClassifiedFires_Training"]],
                                      Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_PercentageRightClassifiedNoFires_Training"]],
                                      1-Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_Empirical_error_Training"]])
    FalsePredictions_Training=100*cbind(1-Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_PercentageRightClassifiedFires_Training"]],
                                    1-Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_PercentageRightClassifiedNoFires_Training"]],
                                    Endlist[[NumberOfCovariates_Lowest_CVk_Overall]][[NumberOfSample_Lowest_CVk_Overall]][[1]][["Training_Result"]][["Mean_Empirical_error_Training"]])
    barplot_RightPercentage_matrix_Training=rbind(RightPredictions_Training, FalsePredictions_Training)
    rownames(barplot_RightPercentage_matrix_Training) <- rbind("Right", "False")
    colnames(barplot_RightPercentage_matrix_Training) <- cbind("50% Fires","50% No Fires","Overall")
    par(mar=c(5, 5, 5, 6)) # bottom, left, top, right
    barplot(barplot_RightPercentage_matrix_Training, main="Training Result",
            col=c("darkgreen","red"),legend = rownames(barplot_RightPercentage_matrix_Training), ylab = "in %",
            args.legend = list(x = "right", title = "Prediction", bty = "n", inset=c(-0.3, -0.3)), border = TRUE)
    abline(h=50, col = "black", lwd= 3, lty = 3)

# pie plot which indicate the different type of errors------
pie_RightFalse_PositivesNegatives_values=round(100*rbind(RightFalse_PositivesNegatives_matrix[1,1], RightFalse_PositivesNegatives_matrix[2,2], 
                                                         RightFalse_PositivesNegatives_matrix[1,2], RightFalse_PositivesNegatives_matrix[2,1]), digits = 1)

pie_RightFalse_PositivesNegatives_names=cbind(rbind(paste(pie_RightFalse_PositivesNegatives_values[1],"%"), 
                                                    paste(pie_RightFalse_PositivesNegatives_values[2], "%"), 
                                                    paste(pie_RightFalse_PositivesNegatives_values[3], "%"), 
                                                    paste(pie_RightFalse_PositivesNegatives_values[4], "%")))
par(mar=c(1, 1, 5, 7)) # bottom, left, top, right
pie(pie_RightFalse_PositivesNegatives_values, pie_RightFalse_PositivesNegatives_names, 
    main = "Final Test Result", col = c("DarkGreen","Green","Red", "DarkRed"))
par(mar=c(1.5,1.5,1.5,1)) # bottom, left, top, right
legend(0.9, 1, c("True Positives","True Negatives","False Positives","False Negatives"), cex = 0.9, 
       fill = c("DarkGreen","Green","Red", "DarkRed"))

# ggplot the CVk mean for different numbers of covariates------
NOC=2:NumberOfCovariates
plotdata=tibble(NOC,CVk_mean_ForAllDifferentNumbersOfCovariates)
ggplot(plotdata, aes(x=NOC, y=CVk_mean_ForAllDifferentNumbersOfCovariates)) + geom_point() + geom_line() + 
  ylim((min(CVk_mean_ForAllDifferentNumbersOfCovariates)-0.05), (max(CVk_mean_ForAllDifferentNumbersOfCovariates)+0.05)) + 
  labs(title= "CVk mean for different numbers of covariates", x="number of covariates", y="CVk mean")


# Save the result------
save.image(file='LogisticRegression6.RData')

  





