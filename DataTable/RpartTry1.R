# install.packages("rpart")
# install.packages("rpart.plot")
# devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')

colnames(train)


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

# Minsplit 

# Minsplit: Min number of observations required to split, prevents over fitting
# if nuumber of observations is node is less than minsplit, the node wont further split
# This prevents overfitting by avoiding ecessive small nodes (Default: 20)

tree <- rpart(Hired ~ Impression+Major+College+Coding, 
              data = train, 
              method = "class",
              control = rpart.control(minsplit = 100)
              )
rpart.plot(tree)

pred <- predict(tree, train, type = "class")
head(prediction)
mean(train$Hired == pred)

pred <- predict(tree, test, type = "class")
head(pred)
mean(test$Hired == pred)

# Min-bucket: This parameneter sets the minimum number of observations required in any terminal(leaf) node of the tree
# If potential split will lead to fewer observations, the split isnt attempted
# Defalult is 1 more than minsplit

tree <- rpart(Hired ~ Impression+Major+College+Coding, 
              data = train, 
              method = "class",
              control = rpart.control(minbucket = 50)
)
rpart.plot(tree)

pred <- predict(tree, train, type = "class")
head(prediction)
mean(train$Hired == pred)

pred <- predict(tree, test, type = "class")
head(pred)
mean(test$Hired == pred)

# CP

# CP: the complexity parameter (cp) is used to control the conplexity of the decision tree
# It specifies the number improvement in the model's accuracy that muct be achieved for a split to be considered
# If the improvement is below cp the split is not attempted, leading to a sipler tree
# igher values of CP result in smaller tress
# the defailt value is 0.01

# Rpart (cp = 0.05)

tree <- rpart(Hired ~ Impression+Major+College+Coding, 
              data = train, 
              method = "class",
              control = rpart.control(cp = 0.05)
)
rpart.plot(tree)

pred <- predict(tree, train, type = "class")
head(prediction)
mean(train$Hired == pred)

pred <- predict(tree, test, type = "class")
head(pred)
mean(test$Hired == pred)

# Rpart (cp = 0.005)

tree <- rpart(Hired ~ Impression+Major+College+Coding, 
              data = train, 
              method = "class",
              control = rpart.control(cp = 0.005)
)
rpart.plot(tree)

pred <- predict(tree, train, type = "class")
head(prediction)
mean(train$Hired == pred)

pred <- predict(tree, test, type = "class")
head(pred)
mean(test$Hired == pred)

########################## THE END #######################################

