##### KNN: Presentation Results #######
source("Code/006_3_KNN_FinalTest.R")
library(tidyverse)
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

# Calculate the mean CVk overview-----
CV_mean_ForAllDifferentNumberOfNearestNeighbours

# mean CVk for all different numbers of covariates
CVk_mean_ForAllDifferentNumbersOfCovariates=matrix(NA, 1, NumberOfCovariates-1)
for(i in 1:(NumberOfCovariates-1)){
  CVk_mean_ForAllDifferentNumbersOfCovariates[i]=mean(CV_mean_ForAllDifferentNumberOfNearestNeighbours[i,])
}

    # Create the names for the columns
    NumberOfCovariates_AsColumnName=rep(NA, (NumberOfCovariates-1))
    for(i in 2:NumberOfCovariates){
      NumberOfCovariates_AsColumnName[[i-1]]=paste(print(i), "Covariates")
    }
    colnames(CVk_mean_ForAllDifferentNumbersOfCovariates)<-NumberOfCovariates_AsColumnName

# mean CVk for all different numbers of nearest neighbours
CVk_mean_ForAllDifferentNumbersOfNearestNeighbours=matrix(NA, 1, length(SequenceOfNearestNeighboursToTest))
for(i in 1:(length(SequenceOfNearestNeighboursToTest))){
  CVk_mean_ForAllDifferentNumbersOfNearestNeighbours[i]=mean(CV_mean_ForAllDifferentNumberOfNearestNeighbours[,i])
}

    # Create the names for the columns
    NumberOfNearestNeighbours_AsColumnName=rep(NA, length(SequenceOfNearestNeighboursToTest))
    for(i in SequenceOfNearestNeighboursToTest){
      NumberOfNearestNeighbours_AsColumnName[[i]]=paste(print(i), "NearestNeighbours")
    }
    colnames(CVk_mean_ForAllDifferentNumbersOfNearestNeighbours)<-NumberOfNearestNeighbours_AsColumnName


# The final presentation list------
PresentationList=list("EmpiricalError_FinalTest"=EmpiricalError_FinalTest, 
                      "RightFalse_PositivesNegatives_matrix"=RightFalse_PositivesNegatives_matrix, 
                      "RightPercentage_matrix"=RightPercentage_matrix, 
                      "NumberOfNearestNeighbours_Lowest_CVk_Overall"=NumberOfNearestNeighbours_Lowest_CVk_Overall,
                      "NumberOfCovariates_Lowest_CVk_Overall"=NumberOfCovariates_Lowest_CVk_Overall,
                      "HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall"=HowToSearchTheNumberOfCovariates_Lowest_CVk_Overall,
                      "Covariates_Lowest_CVk_Overall"=Covariates_Lowest_CVk_Overall,
                      "NumberOfSample_Lowest_CVk_Overall"=NumberOfSample_Lowest_CVk_Overall,
                      "CVk_mean_ForAllDifferentNumbersOfCovariates"=CVk_mean_ForAllDifferentNumbersOfCovariates,
                      "CVk_mean_ForAllDifferentNumbersOfNearestNeighbours"=CVk_mean_ForAllDifferentNumbersOfNearestNeighbours)


# ggplot the CVk mean for different numbers of covariates------
NOC=2:NumberOfCovariates
plotdata_NOC_CVk_mean=tibble(NOC,CVk_mean_ForAllDifferentNumbersOfCovariates)
ggplot(plotdata_NOC_CVk_mean, aes(x=NOC, y=CVk_mean_ForAllDifferentNumbersOfCovariates[1,])) + geom_point() + geom_line() + 
  ylim((min(CVk_mean_ForAllDifferentNumbersOfCovariates)-0.05), (max(CVk_mean_ForAllDifferentNumbersOfCovariates)+0.05)) + 
  labs(title= "CVk mean for different numbers of covariates", x="number of covariates", y="CVk mean")

# barplot the Right-Percentage-matrix-------
RighPredictions=100*cbind(Result_FinalTest$PercentageRightPositives_FinalTest, Result_FinalTest$PercentageRightNegatives_FinalTest, 1-Result_FinalTest$EmpiricalError_FinalTest)
FalsePredictions=100*cbind(1-Result_FinalTest$PercentageRightPositives_FinalTest, 1-Result_FinalTest$PercentageRightNegatives_FinalTest, Result_FinalTest$EmpiricalError_FinalTest)
barplot_RightPercentage_matrix=rbind(RighPredictions, FalsePredictions)
rownames(barplot_RightPercentage_matrix) <- rbind("Right", "False")
colnames(barplot_RightPercentage_matrix) <- cbind(paste(SumOf_Fires, "Fires"), paste(SumOf_NoFires, "No Fires"), paste(SumOf_Observations, "Days"))
par(mar=c(5, 5, 5, 6)) # bottom, left, top, right
barplot(barplot_RightPercentage_matrix, main="Final Test Result",
        col=c("darkgreen","red"),legend = rownames(barplot_RightPercentage_matrix), ylab = "in %",
        args.legend = list(x = "right", title = "Prediction", bty = "n", inset=c(-0.3, -0.3)), border = T)
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
NONN=SequenceOfNearestNeighboursToTest
plotdata_NONN_CVk_mean=tibble(NONN,CVk_mean_ForAllDifferentNumbersOfNearestNeighbours)
ggplot(plotdata_NONN_CVk_mean, aes(x=NONN, y=CVk_mean_ForAllDifferentNumbersOfNearestNeighbours[1,])) + geom_point() + geom_line() + 
  ylim((min(CVk_mean_ForAllDifferentNumbersOfNearestNeighbours)-0.05), (max(CVk_mean_ForAllDifferentNumbersOfNearestNeighbours)+0.05)) + 
  labs(title= "CVk mean for different numbers of nearest neighbours", x="number of nearest neighbours", y="CVk mean")


# Save the result------
save.image(file='KNN_InitialDataset.RData')







