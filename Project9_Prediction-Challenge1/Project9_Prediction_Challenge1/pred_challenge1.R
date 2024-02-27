# install.packages("rpart")
# install.packages("rpart.plot")
# devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

# Reading movies dataset
df<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Tr.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')

# Info for basis
head(df)
summary(df)
# NUmeriical: Audience and Income

unique(df$Genre) # Documentary, Action, Drama, Comedy
unique(df$Content) # PG13, PG, R
unique(df$RATING) # Great, Average

# Creating  columns for identification
df$Quarter_A <- cut(df$Audience, breaks = c(100, 2488.6, 4880.1, 7373.7, 9999), 
                  labels = c("Q1", "Q2", "Q3", "Q4"))
df$Quarter_I <- cut(df$Income, breaks = c(100, 2608.5, 5083.6, 7559.4, 9999), 
                  labels = c("Q1", "Q2", "Q3", "Q4"))
df$log_Income <- log10(df$Income)
df$log_Audience <- log10(df$Audience)
df$sqrt_Income <- sqrt(df$Income)
df$sqrt_Audience <- sqrt(df$Audience)
df$sq_Income <- df$Income^2
df$sq_Audience <- df$Audience^2
df$sub_AI <- df$Audience - df$Income


# Print the first few rows of the updated dataframe
head(df)

# Running basic rpart to get an idea
# RATING ~ Genre + Content + Audience + Income
set.seed(456)

train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Using the monsplit of 100 which is 5% of tatl data of 2000 obersvations
tree_model <- rpart(RATING ~ Genre + Content + Audience + Income, 
                    data = train_data, 
                    control = rpart.control(minsplit = 200)
                    )
tree_model
rpart.plot(tree_model)
# Make predictions on the test data
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == test_data$RATING) / nrow(test_data)
cat("Accuracy:", round(accuracy, 4), "\n") # Accuracy: 0.8918 - Kinda...SUS?


###### Freestple Model

df$Quarter_A <- cut(df$Audience, breaks = c(100, 2488.6, 4880.1, 7373.7, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))
df$Quarter_I <- cut(df$Income, breaks = c(100, 2608.5, 5083.6, 7559.4, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))

head(df)

# The quartile are quite evenly distributed
table(df$Quarter_A, df$Quarter_I)

avg_total <- nrow(df[df$RATING == "Average",])
avg_total # 1324
gr8_total <- nrow(df[df$RATING == "Great",])
gr8_total # 677

head(df)
table(df$RATING, df$Genre)
table(df$RATING, df$Content)
table(df$RATING, df$Quarter_A)
table(df$RATING, df$Quarter_I)


cat("

  Drama has 50 50 split others have 5:2 ratio split
  R has 50 50 split others have 5:2 ratio split (again...)
  Great Rated have Less Attendence (Q1 and Q2)
  Average Rated have more attendence (Q3 and Q4)
  Great Rated movies have more income in percentage (Q3 and Q4)
  Average movies have less income in percentage (Q1 and Q2)

    ")

## SPecific Trials

table(df[df$Content == "PG13",]$RATING, df[df$Content == "PG13",]$Genre)
table(df[df$Content == "PG",]$RATING, df[df$Content == "PG",]$Genre)
table(df[df$Content == "R",]$RATING, df[df$Content == "R",]$Genre)

table(df[df$Genre == "Documentary",]$RATING, df[df$Genre == "Documentary",]$Content)
table(df[df$Genre == "Action",]$RATING, df[df$Genre == "Action",]$Content)
table(df[df$Genre == "Drama",]$RATING, df[df$Genre == "Drama",]$Content)
table(df[df$Genre == "Comedy",]$RATING, df[df$Genre == "Comedy",]$Content)

cat("

  Almost all of the Drama R Movies have Greast Rating
  All other ratios are approx 2:1

    ")

set.seed(456)

train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Using the monsplit of 200 which is 10% of total data of 2000 observations
head(df)
tree_model <- rpart(RATING ~ Genre + Content + Quarter_A + Quarter_I + log_Income + log_Audience + sqrt_Income + sqrt_Audience + sq_Income + sq_Audience + sub_AI, 
                    data = train_data, 
                    control = rpart.control(minsplit = 200)
)
tree_model
rpart.plot(tree_model)
# Make predictions on the test data
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == test_data$RATING) / nrow(test_data)
cat("Accuracy:", round(accuracy, 4), "\n") # Accuracy 0.9052

CrossValidation::cross_validate(train_data, tree_model, 15, 0.9)


###### ###############################################################
##Lets use this R-Part Tree to create out freestyle Model

set.seed(456)
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

decision <- rep('Average', nrow(test_data))
decision[test_data$sub_AI <= -2251 & test_data$log_Audience < 3.6] <- 'Great'
decision[test_data$Quarter_A == "Q3" & test_data$Quarter_I == "Q4"] <- "Average"
decision[test_data$sub_AI > -2251 & test_data$Genre == "Drama" & test_data$Content == "R"] <- 'Great'
decision[test_data$sub_AI < -3096 & test_data$Audience < 4157] <- "Great"
decision

# Some checkings 
accuracy <- sum(decision == test_data$RATING) / nrow(test_data)
cat("Accuracy:", round(accuracy, 4), "\n") # Accuracy 0.9085

######################################################################
# Applying necessary functions to Test Data
head(test)
submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

# Creating  columns for identification
test$Quarter_A <- cut(test$Audience, breaks = c(100, 2488.6, 4880.1, 7373.7, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))
test$Quarter_I <- cut(test$Income, breaks = c(100, 2608.5, 5083.6, 7559.4, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))
test$log_Income <- log10(test$Income)
test$log_Audience <- log10(test$Audience)
test$sqrt_Income <- sqrt(test$Income)
test$sqrt_Audience <- sqrt(test$Audience)
test$sq_Income <- test$Income^2
test$sq_Audience <- test$Audience^2
test$sub_AI <- test$Audience - test$Income
head(test)

# Creating the prediction vector
decision <- rep('Average', nrow(test))
decision[test$sub_AI <= -2251 & test$log_Audience < 3.6] <- 'Great'
decision[test$Quarter_A == "Q3" & test$Quarter_I == "Q4"] <- "Average"
decision[test$sub_AI > -2251 & test$Genre == "Drama" & test$Content == "R"] <- 'Great'
decision[test$sub_AI < -3096 & test$Audience < 4157] <- "Great"
decision

submission$RATINGS <- decision
head(submission)

write.csv(submission, 'D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/Projects/Project9_Prediction_Challenge1/mysubmission.csv', row.names = FALSE)














































































































