# Improved way of doing titles

library(dplyr)

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

# Replace Titles using lookup table
data.combined <- data.combined %>%
  left_join(titles.lookup, by = "Title") # check out joins in cheat sheets (data transformation)
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


# Improve how we impute age


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
View(age.stats)

# Track the ages that were blanks
data.combined$Age.Missing <- ifelse(is.na(data.combined$Age), "Y", "N")
View(data.combined)


# Create subset to impute median
age.lookup <- age.stats %>%
  select(Pclass, Title, Age.Median)
View(age.lookup)


# impute ages
data.combined <- data.combined %>%
  left_join(age.lookup, by = c("Pclass", "Title")) %>%
  mutate(Age = ifelse(Age.Missing == "Y", Age.Median, Age)) %>%
  select(-Age.Median)
View(data.combined)


# Now let's see if we can get any better results



# extra stuff if we end up with enough time


# Take a look at the ticket variable
str(data.combined$Ticket)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


# Investigate Fares
summary(data.combined$Fare)
length(unique(data.combined$Fare))


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")


# Analysis of the cabin variable
str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")






