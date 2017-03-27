
# For Tableau 

# Add this code to the bottom of the titanic_analysis.R script

rf.tableau <- train(Survived ~ Pclass + Title + Family.Size,
                    data = newTrain,
                    method = "rf")
rf.tableau

newTrainNoSurvived <- data.frame(newTrain[c("Pclass", "Title", "Family.Size")])

rf.tableau.prediction <- predict(rf.tableau, newTrainNoSurvived)
rf.tableau.prediction

confusionMatrix(data = rf.tableau.prediction, newTrain$Survived)


rf.tableau.csv <- data.frame(PassengerId = rep(1:891), Survived = rf.tableau.prediction)
write.csv(rf.tableau.csv, file = "rf.tableau.csv", row.names = FALSE)

