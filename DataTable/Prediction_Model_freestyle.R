# Prediction model example

train<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")
head(train)
summary(train)

# Scramble the train frame
v <- sample(1:nrow(train))
v
train_scrambled <- train[v, ]
train_scrambled

# One steap crossvalidation
train_sample <- train_scrambled[nrow(train_scrambled)-200:nrow(train_scrambled),]
train_sample
head(train_sample)

my_prediction <- train_sample

# free style prediction model
# How to test how good the model is
# Observe Error and 

decision <- rep('F', nrow(my_prediction))
decision[my_prediction$Score>50] <- 'D'
decision[my_prediction$Score>60] <- 'C'
decision[my_prediction$Score>70] <- 'B'
decision[my_prediction$Score>80] <- 'A'

my_prediction$Grade <- decision
error <- mean(train_sample$Grade != my_prediction$Grade)
error

# IMPORTANT: DO this checking multiple times and then take average 

# Example 1
test <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022testSNoGrade.csv")
submission <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022submission.csv")

my_prediction <- test
# 
decision <- rep('F', nrow(my_prediction))
decision[my_prediction$Score>50] <- 'D'
decision[my_prediction$Score>60] <- 'C'
decision[my_prediction$Score>70] <- 'B'
decision[my_prediction$Score>80] <- 'A'

submission$Grade <- decision
submission
write.csv(submission, 'submission.csv', row.names = FALSE)

# Example 2

train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')


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

# Model Training 
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

### Example 3


train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')


# Scramble the train frame
v <- sample(1:nrow(train))
v
train_scrambled <- train[v, ]
train_scrambled

train_sample <- train_scrambled[nrow(train_scrambled)-400:nrow(train_scrambled),]
train_sample
head(train_sample)

my_prediction <- train_sample
my_prediction

colnames(train)

# Model in training set
decision <- rep("Yes",nrow(my_prediction))
decision[my_prediction$Impression == "Nerdy" & my_prediction$Major == "IT" & my_prediction$College == "BestCollege"] <- "No"
decision[my_prediction$Impression == "Shy" & my_prediction$Coding == "OK" & my_prediction$College == "BestCollege"] <- "No"
decision[my_prediction$Impression == "Nerdy" & my_prediction$Major == "OK" & my_prediction$College == "BestCollege"] <- "No"
decision[my_prediction$Coding == "Weak"] <- 'No'
decision[my_prediction$Impression == "Nerdy" & my_prediction$Coding == "Weak" & my_prediction$College == "Redbrick"] <- "Yes"


accuracy <- round(mean(my_prediction$Hired==decision),2)
accuracy

# Model in Test Set

decision <- rep("Yes",nrow(test))
decision[test$Impression == "Nerdy" & test$Major == "IT" & test$College == "BestCollege"] <- "No"
decision[test$Impression == "Shy" & test$Coding == "OK" & test$College == "BestCollege"] <- "No"
decision[test$Impression == "Nerdy" & test$Major == "OK" & test$College == "BestCollege"] <- "No"
decision[test$Coding == "Weak"] <- 'No'
decision[test$Impression == "Nerdy" & test$Coding == "Weak" & test$College == "Redbrick"] <- "Yes"


accuracy <- round(mean(test$Hired==decision),2)
accuracy











