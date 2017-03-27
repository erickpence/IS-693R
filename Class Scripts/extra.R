
# Improved way of doing titles

library(dplyr)

# Let's look at the titles a bit deeper
data.combined <- data.combined %>%
  mutate(Title = str_extract(Name, "[a-zA-Z]+\\."))

table(data.combined$Title)



# Create titles subset
titles.subset <- data.frame(Title = c("Mr.", "Capt.", "Col.", "Don.", "Dr.",
                                      "Jonkheer.", "Major.", "Rev.", "Sir.",
                                      "Mrs.", "Dona.", "Lady.", "Mme.", 
                                      "Countess.", 
                                      "Miss.", "Mlle.", "Ms.",
                                      "Master."),
                            New.Title = c(rep("Mr.", 9),
                                          rep("Mrs.", 5),
                                          rep("Miss.", 3),
                                          "Master."),
                            stringsAsFactors = FALSE)
View(titles.subset)


# Join subset with data and replace title
data.combined <- data.combined %>%
  left_join(titles.subset, by = "Title") # check out joins in cheat sheets (data wrangling)
View(data.combined)


data.combined <- data.combined %>%
  mutate(Title = New.Title) %>%
  select(-New.Title)

View(data.combined)

#
# Alternatively, you could do the above in one dplyr pipeline
#
# data.combined <- data.combined %>%
#   left_join(titles.lookup, by = "Title") %>%
#   mutate(Title = New.Title) %>%
#   select(-New.Title)
#


# Check for mistakes

table(data.combined$Title)

data.combined %>%
  filter((Sex == "female" & (Title == "Mr." | Title == "Master.")) |
           (Sex == "male" & (Title == "Mrs." | Title == "Miss.")))



data.combined$Title[data.combined$PassengerId == 797] <- "Mrs."

data.combined$Title <- as.factor(data.combined$Title)


# Improve how we impute age


# Generate statistics for Age variable
age.stats <- data.combined %>%
  group_by(Pclass, Title) %>%
  summarize(Age.Min = min(Age, na.rm = TRUE),
            Age.Max = max(Age, na.rm = TRUE),
            Age.Mean = mean(Age, na.rm = TRUE),
            Age.Median = median(Age, na.rm = TRUE),
            Age.NA.Count = sum(is.na(Age)),
            Age.Var = var(Age, na.rm = TRUE),
            Age.SD = sd(Age, na.rm = TRUE),
            Age.IQR = IQR(Age, na.rm = TRUE)) %>%
  arrange(Title, Pclass)
View(age.stats) # Let's use median


# Track the ages that were blanks
data.combined$Age.Missing <- ifelse(is.na(data.combined$Age), "Y", "N")
View(data.combined)


# Create subset to impute median
age.lookup <- age.stats %>%
  select(Pclass, Title, Age.Median)
View(age.lookup)


# Impute ages based on Pclass and Title
data.combined <- data.combined %>%
  left_join(age.lookup, by = c("Pclass", "Title")) %>%
  mutate(Age = ifelse(Age.Missing == "Y", Age.Median, Age)) %>%
  select(-Age.Median)
View(data.combined)



# Fill in Age NAs with Decision Tree Model
summary(data.combined$Age)



# Set imputed ages back to unknown
data.combined$Age <- ifelse(data.combined$Age.Missing == "Y", "Unknown", data.combined$Age)



# Convert to integer (Unknowns become NA)
data.combined$Age <- as.integer(data.combined$Age)



# Use a decision tree to predict missing ages
AgeEstimate <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Family.Size, 
                data=data.combined[!is.na(data.combined$Age),], method="anova")


data.combined$Age[is.na(data.combined$Age)] <- predict(AgeEstimate, data.combined[is.na(data.combined$Age),])








