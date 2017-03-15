# 
# Modeling in R
#
# Titanic Dataset (Kaggle Version)
#
# 15 March 2017




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
table(data.combined$Survived)


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
males <- data.combined[which(train$Sex == "male"), ]
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
data.combined$Parch <- as.factor(data.combined$Parch)



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











data.combined$Family.Size <- as.factor(temp.sibsp + temp.parch + 1)


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



library(randomForest)


# Let's train a Random Forest model with the default parameters using pclass & title
rf.model.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(4321)
rf.1 <- randomForest(x = rf.model.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)



# Build Random Forest using pclass, title, & sibsp
rf.model.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(4321)
rf.2 <- randomForest(x = rf.model.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)



# Build Random Forest using pclass, title, & parch
rf.model.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(4321)
rf.3 <- randomForest(x = rf.model.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



# Build Random Forest using pclass, title, sibsp, parch
rf.model.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(4321)
rf.4 <- randomForest(x = rf.model.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Build Random Forest using pclass, title, & family.size
rf.model.5 <- data.combined[1:891, c("Pclass", "Title", "Family.Size")]

set.seed(4321)
rf.5 <- randomForest(x = rf.model.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)



# Build Random Forest using pclass, title, sibsp, & family.size
rf.model.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Family.Size")]

set.seed(4321)
rf.6 <- randomForest(x = rf.model.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


# See if you can reduce error using a different combination of variables






# Build Random Forest using pclass, title, parch, & family.size
rf.model.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Family.Size")]

set.seed(4321)
rf.7 <- randomForest(x = rf.model.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)



# Build Random Forest using pclass, sex, age, & family.size
rf.model.8 <- data.combined[1:891, c("Pclass", "Sex", "Age", "Title", "Family.Size")]

set.seed(4321)
rf.8 <- randomForest(x = rf.model.8, y = rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)



# That was fun!



# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Sex", "Age", "Title", "Family.Size")]


# Make predictions
rf.8.predictions <- predict(rf.8, test.submit.df)
table(rf.8.predictions)


# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160215_1.csv", row.names = FALSE)

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
library(doSNOW)


# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# however there are no hard and fast rules - this is where the experience of the 
# Data Scientist (i.e., the "art") comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.


# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549

table(rf.label[cv.10.folds[[33]]])
308 / 494


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3



#==============================================================================
#
# Video #6 - Exploratory Modeling 2
#
#==============================================================================

# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.

# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Per video #5, let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)






# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.2 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 94.9%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 59.6% accuracy.


# Both rpart and rf confirm that title is important, let's investigate further
table(data.combined$title)

# Parse out last name and title
data.combined[1:25, "name"]

name.splits <- str_split(data.combined$name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'?
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) + 
  ggtitle("Surival Rates for new.title by pclass")

# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." | 
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")


# Grab features
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$sex == "female",]

# Update new.title feature
indexes <- which(data.combined$new.title == "Mr." & 
                   data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

# Any other gender slip-ups?
length(which(data.combined$sex == "female" & 
               (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$survived == "1",])
View(first.mr.df[first.mr.df$survived == "1",])

# Take a look at some of the high fares
indexes <- which(data.combined$ticket == "PC 17755" |
                   data.combined$ticket == "PC 17611" |
                   data.combined$ticket == "113760")
View(data.combined[indexes,])

# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = avg.fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# One missing value, take a look
data.combined[is.na(data.combined$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == 1 &
                                       ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again


# OK, let's see if our feature engineering has made any difference
features <- c("pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)