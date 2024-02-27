# install.packages("rpart")
# install.packages("rpart.plot")
# devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

train <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
test <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')

df <- train
head(df)
unique(df$Major)

# Findings 

####### GPA vs Salary
plot(df$GPA, df$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# This data is quite straightlined with 4 stratight lines
# Lets make this data in form of Major segregation
plot(df[df$Major=="Humanities",]$GPA, df[df$Major=="Humanities",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# This seems to be very cloudy distribution - unreliable

plot(df[df$Major=="Other",]$GPA, df[df$Major=="Other",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# This seems to be very cloudy distribution - unreliable

plot(df[df$Major=="STEM",]$GPA, df[df$Major=="STEM",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# DOWNWARD SLOPE - unreliable due to cloudyness

plot(df[df$Major=="Vocational",]$GPA, df[df$Major=="Vocational",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# UPWARD SLOPE - unreliable due to cludines

plot(df[df$Major=="Professional",]$GPA, df[df$Major=="Professional",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# UPWARD SLOPE - unreliable due to cludines

plot(df[df$Major=="Buisness",]$GPA, df[df$Major=="Buisness",]$Salary, 
     main = "Scatterplot of GPA vs Salary", 
     xlab = "GPA", 
     ylab = "Salary")
# RELIABLE FINDING - Lets eyeball the data shall we?
head(df[df$Major=="Buisness" & df$Salary > 10000,]) # Even Number of year
head(df[df$Major=="Buisness" & df$Salary < 10000,]) # Odd number of year

# There is a strong linear correlation between GPA and Majors vs Salary

############ DOB vs Salary
plot(df$DOB, df$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# This data also has straight lines, lets exploit them and find a common ground using Major

plot(df[df$Major == "STEM",]$DOB, df[df$Major == "STEM",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Blur Data - Unreadabe

plot(df[df$Major == "Humanities",]$DOB, df[df$Major == "Humanities",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Blur data - unreadable

plot(df[df$Major == "Other",]$DOB, df[df$Major == "Other",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Underlying still uncreadable 

plot(df[df$Major == "Vocational",]$DOB, df[df$Major == "Vocational",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Blur Unreadable

plot(df[df$Major == "Professional",]$DOB, df[df$Major == "Professional",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Blur Unreadable

plot(df[df$Major == "Buisness",]$DOB, df[df$Major == "Buisness",]$Salary, 
     main = "Scatterplot of DOB vs Salary", 
     xlab = "DOB", 
     ylab = "Salary")
# Distinguisible - PROOF of first Finding

boxplot(Salary ~ Major, data = df, main = "Boxplot of Salary by Major", xlab = "Major", ylab = "Salary")
# The boxplot seems to be very standardized to the Majors (except Others) and some unplesentaries in Business
# The business classification is reasonable due to the fact of previous findings

plot(df$Tuition, df$Salary, main = "Scatterplot of Tuition vs Salary", xlab = "Tuition", ylab = "Salary")
# This plot shows various outliers and gaps between data 
# Lets eyeball the data we get here

df[df$Tuition > 100000,]
# These might be split by Majors here so lets try various majors
unique(df$Major)
df[df$Tuition > 100000 & df$Major == "STEM",] # CLOSE
df[df$Major == "STEM",]
df[df$Tuition > 100000 & df$Major == "Other",] # NOTTTTTTTTTTTT CLOSE
df[df$Tuition > 100000 & df$Major == "Vocational",] # CLOSE
df[df$Tuition > 100000 & df$Major == "Professional",] # CLOSE
df[df$Tuition > 100000 & df$Major == "Humanities",] # CLOSE
df[df$Tuition > 100000 & df$Major == "Buisness",] # CLOSE
# Majors was the right choice here ofc

df[df$Tuition > 50000 & df$Tuition < 100000,]
# These might be split by Majors here so lets try various majors
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "STEM",] # CLOSE
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "Other",] # NOTTTTTTTTT CLOSE
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "Vocational",] # CLOSE
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "Professional",] # CLOSE
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "Humanities",] # CLOSE
df[df$Tuition > 50000 & df$Tuition < 100000 & df$Major == "Buisness",] # CLOSE

# We shall create a tree by this methodology here :- 

# Last but not least the college location
boxplot(Salary ~ College_location, data = df, main = "Boxplot of Salary by College Location", xlab = "Location", ylab = "Salary")
# Lets plot each one individually shall we??
boxplot(Salary ~ Major, data = df[df$College_location == "EastCoast",], main = "Boxplot of Salary by College Location", xlab = "Location", ylab = "Salary")
boxplot(Salary ~ Major, data = df[df$College_location == "WestCoast",], main = "Boxplot of Salary by College Location", xlab = "Location", ylab = "Salary")
boxplot(Salary ~ Major, data = df[df$College_location == "Central",], main = "Boxplot of Salary by College Location", xlab = "Location", ylab = "Salary")

#### Data Formatting
df$square_linkedIN <- df$LinkedIN^2
plot(df$square_linkedIN, df$Salary)
plot(df$LinkedIN, df$Salary, main = "Scatterplot of LinkedIN vs Salary", xlab = "LinkedIN", ylab = "Salary")
head(df)
boxplot(Salary ~ Major, data = df, main = "Boxplot of Salary by Major", xlab = "Major", ylab = "Salary")

# We find no use of College_location and Tuition - lets use those 
plot(df[df$College_location == "EastCoast",]$Tuition, 
     df[df$College_location == "EastCoast",]$Salary, 
     main = "Scatterplot of Tuition vs Salary", 
     xlab = "Tuition", 
     ylab = "Salary")

plot(df[df$College_location == "WestCoast",]$Tuition, 
     df[df$College_location == "WestCoast",]$Salary, 
     main = "Scatterplot of Tuition vs Salary", 
     xlab = "Tuition", 
     ylab = "Salary")

plot(df[df$College_location == "Central",]$Tuition, 
     df[df$College_location == "Central",]$Salary, 
     main = "Scatterplot of Tuition vs Salary", 
     xlab = "Tuition", 
     ylab = "Salary")
# No Colleration due to all straight lnes

print("
      
Lets collect Observations here and put them into one
  1. Other Major has wild distribution of data 
  2. All majors except Other have the same linear distribution vs GPA (LM_model  1/2/3/4)
  3. Business majors have cross distribution of lm based on their GPA and Year [lm model 5/6]
  4. Other Major has correlation with LinkedIN-squared attribute [lm model 7]
  
SO the query stands
  (i) Use Rpart Tree for GPA correlation with 
  (ii) Create lm models with Business major using the GPA and year significance
  (iii) Create lm model of Major Other with correlation with LinkedIN-squared attribute
      
      ")

# TREEEEEEEEE

head(df)
unique(df$Major)

tree_model_1 <- rpart(Salary~GPA, 
                    data = df[df$Major == "STEM",], 
                    method = "anova"
)
tree_model_1
rpart.plot(tree_model_1)

tree_model_2 <- rpart(Salary~GPA, 
                      data = df[df$Major == "Humanities",], 
                      method = "anova"
)
tree_model_2
rpart.plot(tree_model_2)

tree_model_3 <- rpart(Salary~GPA, 
                      data = df[df$Major == "Vocational",], 
                      method = "anova"
)
tree_model_3
rpart.plot(tree_model_3)

tree_model_4 <- rpart(Salary~GPA, 
                      data = df[df$Major == "Professional",], 
                      method = "anova"
)
tree_model_4
rpart.plot(tree_model_4)


lm_model_1 <- lm(Salary ~ square_linkedIN, data = df[df$DOB %% 2 == 0 & df$Major == "Buisness",])
lm_model_2 <- lm(Salary ~ square_linkedIN, data = df[df$DOB %% 2 != 0 & df$Major == "Buisness",])
lm_model_3 <- lm(Salary ~ square_linkedIN, data = df[df$Major == "Other",])
lm_model_4 <- lm(Salary ~ GPA, data = df[df$Major == "STEM",])
lm_model_5 <- lm(Salary ~ GPA, data = df[df$Major == "Humanities",])
lm_model_6 <- lm(Salary ~ GPA, data = df[df$Major == "Vocational",])
lm_model_7 <- lm(Salary ~ GPA, data = df[df$Major == "Professional",])


# Predictions from the decision tree model
pred_tree_1 <- predict(tree_model, newdata = df[df$Major == "STEM",])
pred_tree_2 <- predict(tree_model, newdata = df[df$Major == "Humanities",])
pred_tree_3 <- predict(tree_model, newdata = df[df$Major == "Vocational",])
pred_tree_4 <- predict(tree_model, newdata = df[df$Major == "Professional",])


# Predictions from the linear regression model
pred_lm_1 <- predict(lm_model_1, newdata = df[df$DOB %% 2 == 0 & df$Major == "Buisness",])
pred_lm_2 <- predict(lm_model_2, newdata = df[df$DOB %% 2 == 1 & df$Major == "Buisness",])
pred_lm_3 <- predict(lm_model_3, newdata = df[df$Major == "Other",])
pred_lm_4 <- predict(lm_model_4, newdata = df[df$Major == "STEM",])
pred_lm_5 <- predict(lm_model_5, newdata = df[df$Major == "Humanities",])
pred_lm_6 <- predict(lm_model_6, newdata = df[df$Major == "Vocational",])
pred_lm_7 <- predict(lm_model_7, newdata = df[df$Major == "Professional",])


# Combine predictions into a single vector
combined_predictions <- rep(0, nrow(df))
combined_predictions[df$Major == "STEM"] <- pred_lm_4
combined_predictions[df$Major == "Humanities"] <- pred_lm_5
combined_predictions[df$Major == "Vocational"] <- pred_lm_6
combined_predictions[df$Major == "Professional"] <- pred_lm_7
combined_predictions[df$Major == "Buisness" & df$DOB %% 2 == 0] <- pred_lm_1
combined_predictions[df$Major == "Buisness" & df$DOB %% 2 == 1] <- pred_lm_2
combined_predictions[df$Major == "Other"] <- pred_lm_3


mean((combined_predictions - df$Salary)^2)

# lm - 218 and tree - 1857.63


incomeTest<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')

incomeTest$square_linkedIN <- incomeTest$LinkedIN^2
head(incomeTest)

pred_lm_1 <- predict(lm_model_1, newdata = incomeTest[incomeTest$DOB %% 2 == 0 & incomeTest$Major == "Buisness",])
pred_lm_2 <- predict(lm_model_2, newdata = incomeTest[incomeTest$DOB %% 2 == 1 & incomeTest$Major == "Buisness",])
pred_lm_3 <- predict(lm_model_3, newdata = incomeTest[incomeTest$Major == "Other",])
pred_lm_4 <- predict(lm_model_4, newdata = incomeTest[incomeTest$Major == "STEM",])
pred_lm_5 <- predict(lm_model_5, newdata = incomeTest[incomeTest$Major == "Humanities",])
pred_lm_6 <- predict(lm_model_6, newdata = incomeTest[incomeTest$Major == "Vocational",])
pred_lm_7 <- predict(lm_model_7, newdata = incomeTest[incomeTest$Major == "Professional",])

predictions <- rep(0, nrow(incomeTest))
predictions[incomeTest$Major == "STEM"] <- pred_lm_4
predictions[incomeTest$Major == "Humanities"] <- pred_lm_5
predictions[incomeTest$Major == "Vocational"] <- pred_lm_6
predictions[incomeTest$Major == "Professional"] <- pred_lm_7
predictions[incomeTest$Major == "Buisness" & incomeTest$DOB %% 2 == 0] <- pred_lm_1
predictions[incomeTest$Major == "Buisness" & incomeTest$DOB %% 2 == 1] <- pred_lm_2
predictions[incomeTest$Major == "Other"] <- pred_lm_3

head(predictions)

ids<-c(1:nrow(incomeTest))
submission<-data.frame(ID=ids)
submission$Salary<-predictions
submission[1:10,]

write.csv(submission, 'D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/Projects/Project9_Prediction_Challenge1/REALSUBMISS.csv', row.names = FALSE)

