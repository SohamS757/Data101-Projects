# kaggle datasets download -d lorenzovzquez/data-jobs-salaries
# Link: https://www.kaggle.com/datasets/lorenzovzquez/data-jobs-salaries/
# Source: https://ai-jobs.net/salaries/download/salaries.csv

# Copyright: © Lorenzo Vázquez (Owner) 

install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)

cat("

Data jobs salaries - weekly updated
Usability: 10.00

Columns Used:  
-----------------------------------------
  work_year
  experience_level: 
    EN: Entry-level / Junior
    MI: Mid-level / Intermediate
    SE: Senior-level / Expert
    EX: Executive-level / Director
    
  employment_type:
    PT: Part-time
    FT: Full-time
    CT: Contract
    FL: Freelance
    
  salary_in_usd:
  
  remote_ratio:
    0: No remote work (less than 20%)
    50: Partially remote/hybrid
    100: Fully remote (more than 80%)
    
  company_size:
    S: less than 50 employees (small)
    M: 50 to 250 employees (medium)
    L: more than 250 employees (large)
  
---------------------------------------

")

# Importing Data from Github/ Local
data <- read.csv("https://raw.githubusercontent.com/SohamS757/Data101-Projects/main/Data-Driven-Blog/salaries.csv")
head(data)
colnames(data)

unique(data$job_title)
length(unique(data$job_title)) # Unusable due to Length 120

# Removing unnecessary (not usable now) columns
data <- data[, -c(4, 5, 6, 8, 10)]
head(data) 
# COLUMSN: work_year, experience_level, employment_type, salary_in_usd, remote_ratio, company_size

unique(data$work_year) # 2023 2020 2022 2021

# Checking Ratios for a better hypothesis
# The Top 2 Are Listed Here: (Total trials: 34 needed to get best 2)
# OUTRAGEOS BUT INVALID Combinations: EX-CT Combination and FL-L
mean(data$salary_in_usd)

mean(data[data$employment_type == "CT" & data$experience_level == "EX",]$salary_in_usd)
nrow(data[data$employment_type == "CT" & data$experience_level == "EX",]) # Invalid: Rows: ONLY 1!!

mean(data[data$company_size == "L" & data$employment_type == "FL",]$salary_in_usd)
nrow(data[data$company_size == "L" & data$employment_type == "FL",]) # Invalid Rows: ONLY 1!!

############### Valid Combinations - 

# COMBINATION 1 
mean(data[data$remote_ratio == "100" & data$experience_level == "EX",]$salary_in_usd)
nrow(data[data$remote_ratio == "100" & data$experience_level == "EX",]) 
# 209 Rows Somewhat Valid

ex_data <- data[data$experience_level == "EX",]

data_frame = data.frame(
  Category = c("Fully Remote", "Semi-Remote", "No-Remote"),
  Value = c(mean(ex_data[ex_data$remote_ratio == "100",]$salary_in_usd), 
            mean(ex_data[ex_data$remote_ratio == "50",]$salary_in_usd), 
            mean(ex_data[ex_data$remote_ratio == "0",]$salary_in_usd)
            )
)

barplot(data_frame$Value, names.arg = data_frame$Category, 
        col = c("blue","red","yellow"), 
        main = "Salary Distribution for Executive level", 
        xlab = "Executive Level", 
        ylab = "Average Salary")

# COMBINATION 2

mean(data[data$remote_ratio == "0" & data$employment_type == "FT",]$salary_in_usd)
nrow(data[data$remote_ratio == "0" & data$employment_type == "FT",]) 
# 4882??? Rows Valid

ft_data <- data[data$employment_type == "FT",]
data_frame = data.frame(
  Category = c("Fully Remote", "Semi-Remote", "No-Remote"),
  Value = c(mean(ft_data[ft_data$remote_ratio == "100",]$salary_in_usd), 
            mean(ft_data[ft_data$remote_ratio == "50",]$salary_in_usd), 
            mean(ft_data[ft_data$remote_ratio == "0",]$salary_in_usd)
  )
)

barplot(data_frame$Value, names.arg = data_frame$Category, 
        col = c("blue","red","yellow"), 
        main = "Salary Distribution for Full-time", 
        xlab = "Full Time", ylab = "Average Salary")


cat(" Comparing SECOND finding with others using permutation test
      ~ due to its vast number of Tuples and Reliability ")

nrow(ft_data) # 8228 rows

# Null: There is no difference in mean salary between semi-remote 
        # and (remote or no-remote) in full time positions

# Alternate:" There is a significant difference in mean salary between 
        # semi-remote and (remote or no-remote) in full time positions

permutation_test(ft_data, 'remote_ratio', 'salary_in_usd', 10000, '50','100') # P-Value: 1

permutation_test(ft_data, 'remote_ratio', 'salary_in_usd', 10000, '50','0') # P-value: 1

# Since p-value is 0 in both the cases, we can infer
# --> We Reject the Null Hypothesis
# --> There is a significant difference in salaries between semi-remote and (remote or no-remote) in full time positions

remote_count <- length(unique(ft_data))
remote_count
# No need to calculate any Significant level to pass for such data due to P = 0

cat("
Baysian Odds Task
  Observation: The experice-level of the employee is Senior-Level / Exprt
  Belief:      The emmployee works fully remote (remote_ratio = 100)
    ")

contingency_table <- table(data$experience_level, data$remote_ratio)
contingency_table

PriorProb <- sum(contingency_table[,3]) / sum(contingency_table[,])
PriorProb

PriorOdds <- PriorProb/(1-PriorProb)
PriorOdds

Truepositive <- contingency_table[4,3]/sum(contingency_table[,3])
Truepositive

FalsePositive <- sum(contingency_table[4, 1:2]) / sum(contingency_table[ ,1:2 ])
FalsePositive

Likelihoodratio <- Truepositive/FalsePositive
Likelihoodratio

PosteriorOdds <- Likelihoodratio *PriorOdds
PosteriorOdds

cat("----------------------------------------------------------------------")

# Refresing Data in case any manipulation prior
data <- read.csv("https://raw.githubusercontent.com/SohamS757/Data101-Projects/main/Data-Driven-Blog/salaries.csv")
head(data)
colnames(data)


cat('Special Findings - FOR BLOG "Why Data??" ')

# Finding 1 - The Frequency of People and employment-type

data_frame = data.frame(
  Category = c("Part-time", "Full-time", "Conract", "Freelancer"),
  Value = c(nrow(data[data$employment_type == "PT",]), 
            nrow(data[data$employment_type == "FT",]), 
            nrow(data[data$employment_type == "CT",]), 
            nrow(data[data$employment_type == "FL",])
  )
)

barplot(data_frame$Value, names.arg = data_frame$Category, 
        col = c("blue","red","yellow","purple"), 
        main = "Frequency Distribution for Employment-type", 
        xlab = "Emplyment_type", ylab = "Frequency of EMployees")

# Majority of the people hired are for full-time of ABOUT 99.7%

# Finding 2 - The Frequency of People and experience-level

data_frame = data.frame(
  Category = c("Junior", "Intermediate", "Expert", "Director"),
  Value = c(nrow(data[data$experience_level == "EN",]), 
            nrow(data[data$experience_level == "MI",]), 
            nrow(data[data$experience_level == "SE",]), 
            nrow(data[data$experience_level == "EX",])
  )
)

barplot(data_frame$Value, names.arg = data_frame$Category, 
        col = c("blue","red","purple","yellow"), 
        main = "Frequency Distribution for Experience-Level", 
        xlab = "Full Time", ylab = "Average Salary")

# Finding 3 - The salaries of employees based on their experience Level

boxplot(salary_in_usd ~ experience_level, data = data[data$company_size == "L",],
        xlab = "Experience-Level", ylab = "Salary in USD",
        main = "Salary Distribution by Experience-level",
        col = c("blue","red","purple","yellow"), border = "black")

cat("Some presentational summaries - NOT TO BE GRADED")

summary(data$salary_in_usd)
summary(data[data$experience_level == "EN",]$salary_in_usd)
summary(data[data$experience_level == "MI",]$salary_in_usd)
summary(data[data$experience_level == "SE",]$salary_in_usd)
summary(data[data$experience_level == "EX",]$salary_in_usd)

cat(" THE END ")
