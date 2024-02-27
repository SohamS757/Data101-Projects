# Recitation 12
# Linear Regression: 
# is a statistical modeling technique that aims to establish a linear relationship 
# between a dependent variable (the target variable we want to predict) and one or more independent variables.

# Example 1
# Load the dataset
housingData <- read.csv('housingData.csv')

# Explore the dataset
str(housingData)  # Check the structure of the dataset
summary(housingData)  # Summary statistics

# Train-test split
set.seed(123)  # Set seed for reproducibility
split <- 0.7 * nrow(housingData)
housingTrain <- housingData[1:split, ]
housingTest <- housingData[(split + 1):nrow(housingData), ]

# Building the linear regression model
lm_model <- lm(Price ~ SquareFeet + Bedrooms + Bathrooms, data = housingTrain)

# Making predictions on the test set
predictions <- predict(lm_model, newdata = housingTest)

# Calculating MSE
library(Metrics)
mse_value <- mse(housingTest$Price, predictions)
print(paste("Mean Squared Error:", mse_value))

# Building a decision tree model (rpart)
library(rpart)
library(rpart.plot)

tree_model <- rpart(Price ~ SquareFeet + Bedrooms + Bathrooms, data = housingTrain, method = 'anova')
rpart.plot(tree_model)
tree_predictions <- predict(tree_model, newdata = housingTest)

tree_mse <- mse(housingTest$Price, tree_predictions)
print(paste("Decision Tree Mean Squared Error:", tree_mse))


# Example 2
# Load the dataset
car_data <- read.csv('carData.csv')

# Explore the dataset
str(car_data)  # Check the structure of the dataset
summary(car_data)  # Summary statistics

# Train-test split (70-30 ratio)
set.seed(123)  # Set seed for reproducibility
split <- 0.7 * nrow(car_data)
train_data <- car_data[1:split, ]
test_data <- car_data[(split + 1):nrow(car_data), ]

# Building the linear regression model
lm_model <- lm(price ~ horsepower + citympg + highwaympg + carheight + carwidth + carlength + cylindernumber, data = train_data)

# Making predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Calculating MSE
library(Metrics)
mse_value <- mse(test_data$price, predictions)
print(paste("Mean Squared Error:", mse_value))

# Building a decision tree model (rpart)
library(rpart)
library(rpart.plot)

tree_model <- rpart(price ~ horsepower + citympg + highwaympg + carheight + carwidth + carlength + cylindernumber, data = train_data, method = 'anova')
rpart.plot(tree_model)
tree_predictions <- predict(tree_model, newdata = test_data)

tree_mse <- mse(test_data$price, tree_predictions)
print(paste("Decision Tree Mean Squared Error:", tree_mse))

