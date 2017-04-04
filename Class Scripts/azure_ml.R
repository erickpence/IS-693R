#
# Copy this code into Azure ML "Execute R Script" Module
#
# Replace train with the name of the dataset

library(ggplot2)

# Visualize survival rates by Pclass
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# Add title variable
train$Title <- ifelse(grepl('Mr. ', train$Name),'Mr',
                    ifelse(grepl('Mrs. ', train$Name),'Mrs',
                    ifelse(grepl('Miss.', train$Name),'Miss', 
                    ifelse(grepl('Master.', train$Name),'Master',
                    'Other'))))

# Visualize survival rates by Title, Pclass
ggplot(train, aes(x = Title, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


