
library(devtools)
library(HypothesisTesting)

# Reading the dataframe
df <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Miderm.csv")
head(df)
summary(df)

# Splitiign dataset and adding a coulumn corresponding to the quartile 
# Cutting based on Min, Q1, Mediam, Q3 and Max
df$Quarter <- cut(df$Age, breaks = c(18.02, 38.93, 60.01, 80.07, 100), 
                  labels = c("Q1", "Q2", "Q3", "Q4"))
head(df)

########################################################################
cat("##################### PART A")

table_NetworthVParty<- table(df$Party, df$NetWorth) # Significant Observation - No further expansion
table_NetworthVParty

table_EducationvParty <- table(df$Party, df$Education) # Significant Observation - No further expansion
table_EducationvParty

table_LocationvParty <- table(df$Party, df$Location) # Insignificant Observations
table_LocationvParty

table_QaurtervParty <- table(df$Party, df$Quarter) # Insignificant Observation
table_QaurtervParty

## Expansion of Queries

table_LocationvParty_Q1 <- table(df[df$Quarter == "Q1",]$Party, df[df$Quarter == "Q1",]$Location) 
table_LocationvParty_Q1

table_LocationvParty_Q2 <- table(df[df$Quarter == "Q2",]$Party, df[df$Quarter == "Q2",]$Location) 
table_LocationvParty_Q2

table_LocationvParty_Q3 <- table(df[df$Quarter == "Q3",]$Party, df[df$Quarter == "Q3",]$Location) 
table_LocationvParty_Q3

table_LocationvParty_Q4 <- table(df[df$Quarter == "Q4",]$Party, df[df$Quarter == "Q4",]$Location) 
table_LocationvParty_Q4

table_QaurtervParty_Central <- table(df[df$Location == "Central",]$Party, df[df$Location == "Central",]$Quarter) 
table_QaurtervParty_Central

table_QaurtervParty_East <- table(df[df$Location == "EastCoast",]$Party, df[df$Location == "EastCoast",]$Quarter) 
table_QaurtervParty_East

table_QaurtervParty_West <- table(df[df$Location == "WestCoast",]$Party, df[df$Location == "WestCoast",]$Quarter) 
table_QaurtervParty_West

# Getting a hang of votes to understand the data
nrow(df[df$Party == "Republican",]) # 1420
nrow(df[df$Party != "Republican",]) # 580


########## My eyeball Observations through tables

# 1 - People with High Income ALL Voted for Republicans 661 to 0 (SIGNIFINACT)
# 2 - People with Primary School Educations Mostly voted for Republicans 595 to 58 ((SIGNIFINACT))

# 3 - People in Q2 Q3 and Q4 voted mostly for Republicans abour 3:1 Ratio (INSIGNIFINACT)

# Scramble the data frame
v <- sample(1:nrow(df))
v
df_scrambled <- df[v, ]
df_scrambled

df_sample <- df_scrambled[nrow(df_scrambled)-400:nrow(df_scrambled),]
df_sample
head(df_sample)

my_prediction <- df_sample
my_prediction

colnames(df_sample)

# Creating decision vector to implement prediction
decision <- rep("Democrat",nrow(my_prediction))

decision[my_prediction$NetWorth == "High"] <- 'Republican'
decision[my_prediction$Education == "PrimarySchool"] <- 'Republican'

accuracy <- round(mean(my_prediction$Party==decision),2)
accuracy # 0.77 which is 77% approved for full credit


########################################################################
cat("##################### PART B")

# Since we know the fact that the Q1 is the only quarter split between R and D
# We use this subset as calulating mean of Democrat Age 

mean(df$Age) # 60.011
mean(df[df$Party == "Democrat",]$Age) # 54.09 < 60.011 - 3
nrow(df[df$Party == "Democrat",]) # 580 > 300

# Since there are only 2 varables in Party, we donot need bonferenni coefficient here
permutation_test(df, "Party", "Age", 10000, "Democrat", "Republican") # P-value: 0
cat("Reject the Null Hypothesis 
    0 < 0.05 (aploha)
    Null Hypotheis: 
    There is no significant difference between ages who voted for Republicans and Democrats
")

# Lets try several subsets with Republican Majority

mean(df[df$Education == "PrimarySchool" ,]$Age) # 61.05 Failure
mean(df[df$Education == "PrimarySchool" & df$Party == "Republican",]$Age) # 64.192 > 60.011 + 2
nrow(df[df$Education == "PrimarySchool" & df$Party == "Republican",]) # 595 > 300

# Using bonferroni coefficient since more than two variables in Education
unique_education <- length(unique(df$Education))

m_education <- unique_education * (unique_education - 1)/2
education_significance <- 0.05/m_education
education_significance # 0.0167

# Since there are only 2 varables in Party, we donot need bonferenni coefficient here

permutation_test(df[df$Education == "PrimarySchool",], "Party", "Age", 10000, "Democrat", "Republican") # P-value: 0
cat("Reject the Null Hypothesis 
    0 < 0.0167
    Null Hypotheis: 
    There is no significant difference between ages who voted for Republicans and Democrats if they were 
")


########################################################################
cat("##################### PART C")

elect <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Miderm.csv")


# Given COde for Part C
Net<-unique(elect$NetWorth)
Edu <-unique(elect$Education) 
Loc<-unique(elect$Location)
Party<-unique(elect$Party)

Netseq <- sample(Net, 20000, replace = TRUE)
Eduseq<-sample(Edu, 20000, replace = TRUE)
Locseq<-sample(Loc, 20000, replace = TRUE)
Partseq<-sample(Party, 20000, replace = TRUE)
Ageseq<- runif(20000, 18, 100)
colnames(elect)
electR<-data.frame(NetWorth=Netseq, Age=Ageseq, Education=Eduseq,Location=Locseq, Party=Partseq)

set.seed(757)

S <- electR[sample(nrow(electR), 300), ]

overall_proportion <- table(electR$Party) / nrow(electR)

subset_proportion <- table(elect$Party) / nrow(S)

# Print the overall and subset proportions
print("Overall Proportions:")
overall_proportion

cat("Subset Proportions:")
subset_proportion

chi_squared <- chisq.test(table(S$Party), p = overall_proportion)
cat("Chi-squared test:")
chi_squared 
# p-value - 0.7786 OR 0.2214
# Since the p value is greater than significant level (0.2214 > 0.05) 
#   there is no significant difference and is just a random fluke


########################################################################
cat("##################### PART D")

market <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/HomeworkMarket2022.csv")
head(market)
summary(market)

unique(market$SoftDrinks) # Sprite, Orange Juice, None, Cola
unique(market$Wine) # White, Red, None
unique(market$Snacks) # None, Potato Chips, Pretzels, Popcorn, Crackers
unique(market$Sweets) # Snickers, Milky Way, Twix


# Part 1 P-value
contingency_table <- table(market$Location, market$Beer)
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # p-value: 0.7375 (Or 1 - 0.7375 = 0.2625)

# Expected Contingency Table from Chi-squared-test
chi_squared_test$expected

# Observed Contingency Table from Chi-squared-test
chi_squared_test$observed

# Creating other contingency Tables to identity the object
contingency_table <- table(market$Location, market$SoftDrinks)
contingency_table
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # pval: 0.07

contingency_table <- table(market$Location, market$Wine)
contingency_table
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # pval: 0.5551

contingency_table <- table(market$Location, market$Sweets)
contingency_table
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # pvalue: 0.0132


#### SUBSET
contingency_table <- table(market$Location, market$Snacks)
contingency_table
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # pvalue: 1.86e-14 MAJORITY CHI SQUARES: NONE

# Since NONE produces the most chi-squared value, we will use it as a factor
#     and agan calculate the contingency table and chisquared value
contingency_table <- table(market[market$Snacks == "None",]$Location, market[market$Snacks == "None",]$Beer)
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test # P-value: 2.2 e-16

# When NONE Snack is purchased , the location and beer have a concrete correlation
# Expected Contingency_Table

contingency_table <- table(market[market$Snacks == "None",]$Location, market[market$Snacks == "None",]$Beer)
chi_squared_test <- chisq.test(contingency_table)

chi_squared_test$expected















































































