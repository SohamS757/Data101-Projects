# install.packages("rpart")
# install.packages("rpart.plot")
# devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

# Reading movies dataset
movies<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Tr.csv')
df <- movies
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')

# Info for basis
head(df)
summary(df)
# NUmeriical: Audience and Income

unique(df$Genre) # Documentary, Action, Drama, Comedy
unique(df$Content) # PG13, PG, R
unique(df$RATING) # Great, Average

# Creating new columns to use RPART 
df$log_add <- log10(df$Income) + log10(df$Audience)
df$sub_AI <- df$Audience - df$Income
df$div_AI <- df$Audience / df$Income
df$mul_AI <- df$Audience * df$Income
df$sqrt_Income <- sqrt(df$Income)
df$sqrt_Audience <- sqrt(df$Audience)
df$sq_Income <- df$Income^2
df$sq_Audience <- df$Audience^2
df$Quarter_A <- cut(df$Audience, breaks = c(100, 2488.6, 4880.1, 7373.7, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))
df$Quarter_I <- cut(df$Income, breaks = c(100, 2608.5, 5083.6, 7559.4, 9999), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))

set.seed(456)

train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Using the monsplit of 100 which is 5% of tatl data of 2000 obersvations
tree_model <- rpart(RATING ~ Genre + Content + Audience + Income + log_add + sub_AI + div_AI + mul_AI, 
                    data = train_data, 
                    control = rpart.control(minsplit = 200)
)
tree_model
rpart.plot(tree_model)
# Make predictions on the test data
predictions <- predict(tree_model, newdata = test_data, type = "class")
predictions

# Calculate accuracy
accuracy <- sum(predictions == test_data$RATING) / nrow(test_data)
cat("Accuracy:", round(accuracy, 4), "\n") # Accuracy: 0.9484 VERY IMPRESSIVE with NON-overfitiing

# Running Crossvalidation to get errors
CrossValidation::cross_validate(train_data, tree_model, 15, 0.9)

############################################################################

# Kaggle Submission


# Applying necessary functions to Test Data
head(test)
submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

test$log_add <- log10(test$Income) + log10(test$Audience)
test$sub_AI <- test$Audience - test$Income
test$div_AI <- test$Audience / test$Income
test$mul_AI <- test$Audience * test$Income

tree_model <- rpart(RATING ~ Genre + Content + Audience + Income + div_AI + mul_AI, 
                    data = df, 
                    control = rpart.control(minsplit = 200)
)
tree_model
rpart.plot(tree_model)
# Make predictions on the test data
predictions <- predict(tree_model, newdata = test, type = "class")
predictions

submission$RATING <- predictions
head(submission)
write.csv(submission, 'D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/Projects/Project9_Prediction_Challenge1/mysubmission.csv', row.names = FALSE)






