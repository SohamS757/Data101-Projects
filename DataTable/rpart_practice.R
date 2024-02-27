install.packages("rpart")
install.packages("rpart.plot")
devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')

colnames(train)

###### FREESTYLE - LAST TIME (LOOK OVER)

# Scramble the train frame
v <- sample(1:nrow(train))
v
train_scrambled <- train[v, ]
train_scrambled

train_sample <- train_scrambled[nrow(train_scrambled)-200:nrow(train_scrambled),]
train_sample
head(train_sample)

my_prediction <- train_sample
my_prediction

colnames(train)
decision <- rep("No",nrow(my_prediction))
decision[my_prediction$College != "BestCollege" & my_prediction$Coding != "Weak"] <- "Yes"
decision[my_prediction$Major != "IT" & my_prediction$Impression != "Outgoing "& my_prediction$Coding == "Excellent"] <- "Yes"
accuracy <- round(mean(my_prediction$Hired==decision),2)
accuracy

# Model Testing 
decision <- rep("No",nrow(test))
decision[test$College != "BestCollege" & test$Coding != "Weak"] <- "Yes"
decision[test$Major != "IT" & test$Impression != "Outgoing "& test$Coding == "Excellent"] <- "Yes"
accuracy <- round(mean(test$Hired==decision),2)
accuracy

# rpasrt: (formula, method, data, control...)

tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train, method = "class")
rpart.plot(tree)

pred <- predict(tree, train, type = "class")
head(prediction)
mean(train$Hired == pred)

pred <- predict(tree, test, type = "class")
head(pred)
mean(test$Hired == pred)


# Cross Validation

CrossValidation::cross_validate(train, tree, 15, 0.9)




























