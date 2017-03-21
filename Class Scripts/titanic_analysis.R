# 
# Modeling in R
#
# Titanic Dataset (Kaggle Version)


# Packages to install
library(ggplot2)
library(stringr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(caret)
library(e1071)
library(dplyr)



# Load titanic data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)


# Add a 'Survived' variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("Unknown", nrow(test)), test[,])


# Combine data sets
data.combined <- rbind(train, test.survived)


# Check data types
str(data.combined)


# Convert Survived and Pclass to factors
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Examine survival rates (contingency table)
table(data.combined$Survived) # naive rule


# Distribution of classes
table(data.combined$Pclass)


library(ggplot2)


# Visualize survival rates by pclass
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 



# See what the first few names look like
head(as.character(train$Name))



# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)



# Investigate misses
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]



# Investigate mrses
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5,]



# Investigate males
males <- data.combined[which(data.combined$Sex == "male"), ]
males[1:5,]



# Add title variable
data.combined$Title <- ifelse(grepl('Mr. ',data.combined$Name),'Mr',ifelse(grepl('Mrs. ',data.combined$Name),'Mrs',ifelse(grepl('Miss.',data.combined$Name),'Miss', ifelse(grepl('Master.', data.combined$Name),'Master','Other'))))
data.combined$Title <- as.factor(data.combined$Title)

View(data.combined)



# Visualize potential relationships 
# We only have survived values for training data, so just use first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Pretty cool, we added a new feature!



# Look at gender distribution
table(data.combined$Sex)



# Visualize the relationship of sex, pclass, and survival
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")



# Check age distribution 
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])



# Lot's of NAs...what should we do?


# Impute NA values with the median age
data.combined$Age[is.na(data.combined$Age)] <- median(data.combined$Age, na.rm=TRUE)


# Maybe we could have been more accurate by using titles?


# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)



# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)



# So maybe title is predictive. Visualize survival rates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")



# Convert the parch variable to a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")



# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)


# Alright! See if you can create a factor variable to calculate the family size (Family.Size) using the above variables






View(data.combined)



# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = Family.Size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")



# Due to time, let's not worry about the other variables




####################################### Modeling ##############################################

# Logistic Regression

logreg1 <- glm(Survived ~ Pclass + Title, 
               data = data.combined[1:891, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Title", "Parch", "Family.Size")],
               family = binomial(link = 'logit'))
summary(logreg1)


# Decision Tree 
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)



# Create a single decision tree model
rpart1 <- rpart(Survived ~ Pclass + Sex + Age + Title + Family.Size,
                data = data.combined[1:891, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Title", "Parch", "Family.Size")],
                method = "class")



# Visualize the decision tree using fancy tree
fancyRpartPlot(rpart1)



# Create test dataframe
test.submit <- data.combined[892:1309, c("Title", "Pclass", "Family.Size", "Age", "Sex", "SibSp", "Parch")]



# Make predictions and submit for scoring
rpart1.prediction <- predict(rpart1, test.submit, type = "class")
table(rpart1.prediction)
rpart1.submit <- data.frame(PassengerId = rep(892:1309), Survived = rpart1.prediction)
write.csv(rpart1.submit, file = "rpart1.csv", row.names = FALSE)


# Scored 76.55 (not great)



########### Random Forest #############

library(randomForest)


# Let's train a Random Forest model with the default parameters using pclass & title
rf.model.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(123)
rf.1 <- randomForest(x = rf.model.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)



# Build Random Forest using pclass, title, & sibsp
rf.model.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(123)
rf.2 <- randomForest(x = rf.model.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)



# Build Random Forest using pclass, title, & parch
rf.model.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(123)
rf.3 <- randomForest(x = rf.model.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



# Build Random Forest using pclass, title, sibsp, parch
rf.model.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(123)
rf.4 <- randomForest(x = rf.model.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Build Random Forest using pclass, title, & family.size
rf.model.5 <- data.combined[1:891, c("Pclass", "Title", "Family.Size")]

set.seed(123)
rf.5 <- randomForest(x = rf.model.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)



# Build Random Forest using pclass, title, sibsp, & family.size
rf.model.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Family.Size")]

set.seed(123)
rf.6 <- randomForest(x = rf.model.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


# See if you can reduce error using a different combination of variables







# Build Random Forest using pclass, title, parch, & family.size
rf.model.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Family.Size")]

set.seed(123)
rf.7 <- randomForest(x = rf.model.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)



# Build Random Forest using pclass, sex, age, & family.size
rf.model.8 <- data.combined[1:891, c("Pclass", "Sex", "Age", "Title", "Family.Size")]

set.seed(123)
rf.8 <- randomForest(x = rf.model.8, y = rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)



# Create test dataframe for submission
test.submit <- data.combined[892:1309, c("Title", "Pclass", "Family.Size", "Age", "Sex", "SibSp", "Parch")]



# Make predictions and submit for scoring
rf.5.prediction <- predict(rf.5, test.submit, type = "class")
table(rf.5.prediction)
rf.5.submit <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.prediction)
write.csv(rf.5.submit, file = "rf.5.csv", row.names = FALSE)


# Scored 77.99  rf.8

# Scored 79.43  rf.5



# That was fun!



#################### Evaluating Model Performance ####################
library(caret)
library(e1071)


# Split train data into training and validation sets

# Create dataset for rows with Survived values
newTrain <- data.combined[1:891,]


# Split data 70% train 30% validate
splitTrain <- createDataPartition(newTrain$Survived, p = .7, list = FALSE)


# Survived Factor has 3 levels but should be 2
newTrain$Survived <- droplevels(newTrain$Survived)


# Create train and test subsets
trainData <- newTrain[splitTrain,]
testData <- newTrain[-splitTrain,]

nrow(trainData)
nrow(testData)


# Build models with CARET
set.seed(123)

rf.9 <- train(Survived ~ Pclass + Title,
              data = trainData,
              method = "rf")
rf.9
  

rf.9.prediction <- predict(rf.9, testData)
rf.9.prediction

confusionMatrix(data = rf.9.prediction, testData$Survived)




# Cross-validation
set.seed(123)

ctrl.1 <- trainControl(method = "repeatedcv", repeats = 3, number = 5)

rf.10 <- train(Survived ~ Pclass + Title,
              data = newTrain,
              method = "rf",
              trControl = ctrl.1)
rf.10

confusionMatrix(rf.10)

rf.10.prediction <- predict(rf.10, testData)
rf.10.prediction

confusionMatrix(data = rf.10.prediction, testData$Survived)



# What does Regression model output look like? Let's try to predict Fare (Decison Tree Regression)
rpart.2 <- train(Fare ~.,
               data = newTrain,
               method = "rpart",
               trControl = ctrl.1)
rpart.2





















