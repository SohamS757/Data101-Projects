# Midterm Examination

# Importing Required Modules
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

# Extracting the data from dataset
df <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/electMidterm.csv")
summary(df)
head(df)

unique(df$NetWorth) # High, Low Medium
unique(df$Education) # College, HighSchool, PrimarySchool
unique(df$Location) # Central, WestCoast, EastCoast
unique(df$Party) # Republican, Democrat
unique(df$Age) # Numbers

########## PART A

# Taking the ratios of the following values to visualize the difference
nrow(df[df$Education == "PrimarySchool" & df$Party == "Democrat",]) / nrow(df[df$Education == "PrimarySchool" & df$Party != "Democrat",])
nrow(df[df$Education == "HighSchool" & df$Party == "Democrat",]) / nrow(df[df$Education == "HighSchool" & df$Party != "Democrat",])
nrow(df[df$Education == "College" & df$Party == "Democrat",]) / nrow(df[df$Education == "College" & df$Party != "Democrat",])
# 1.003, 0.9473, 1.1347

nrow(df[df$NetWorth == "High" & df$Party == "Democrat",]) / nrow(df[df$NetWorth == "High" & df$Party != "Democrat",])
nrow(df[df$NetWorth == "Low" & df$Party == "Democrat",]) / nrow(df[df$NetWorth == "Low" & df$Party != "Democrat",])
nrow(df[df$NetWorth == "Medium" & df$Party == "Democrat",]) / nrow(df[df$NetWorth == "Medium" & df$Party != "Democrat",])
# 1.0712, 0.9970, 1.012

nrow(df[df$Location == "Central" & df$Party == "Democrat",]) / nrow(df[df$Location == "Central" & df$Party != "Democrat",])
nrow(df[df$Location == "EastCoast" & df$Party == "Democrat",]) / nrow(df[df$Location == "EastCoast" & df$Party != "Democrat",])
nrow(df[df$Location == "WestCoast" & df$Party == "Democrat",]) / nrow(df[df$Location == "WestCoast" & df$Party != "Democrat",])
# 1.025, 1.026, 0??????

# First Pair - tuples count above 300 Satisfied-
nrow(df[df$Location == "WestCoast" & df$Party == "Democrat",]) # 0????
nrow(df[df$Location == "WestCoast" & df$Party != "Democrat",]) # 331

# Lets look at age ratio of ones above 60 and onles below 

nrow(df[df$Age >= 90 & df$Party == "Democrat",]) / nrow(df[df$Age >= 90 & df$Party != "Democrat",])
nrow(df[df$Age <= 19 & df$Party == "Democrat",]) / nrow(df[df$Age <= 19 & df$Party != "Democrat",])

# SET 1
nrow(df[df$Location == "EastCoast" & df$Party == "Democrat" & df$Age < 60,]) / 
  nrow(df[df$Location == "EastCoast" & df$Party != "Democrat" & df$Age < 60,]) # ABSOLUTE 0!!!!

nrow(df[df$Location == "EastCoast" & df$Party == "Democrat" & df$Age < 60,]) # 0 ROWS
nrow(df[df$Location == "EastCoast" & df$Party != "Democrat" & df$Age < 60,]) # 333 ROWS

# SET 2
nrow(df[df$Location == "WestCoast" & df$Party == "Democrat" & df$Age > 60,]) / 
  nrow(df[df$Location == "WestCoast" & df$Party != "Democrat" & df$Age > 60,]) # 0.0151 ABOUT 7 TIMES!!

nrow(df[df$Location == "WestCoast" & df$Party == "Democrat" & df$Age > 60,]) # 5 ROWS
nrow(df[df$Location == "WestCoast" & df$Party != "Democrat" & df$Age > 60,]) # 331 ROWS

############################## PART B
# mean age of data set
mean(df$Age) 

# Finding mean age of different subsets
mean(df[df$Location == "Central" & df$Party == "Democrat",]$Age) # 61.32
mean(df[df$Location == "EastCoast" & df$Party == "Democrat",]$Age) # 80.05 > 60.011 + 18
mean(df[df$Location == "WestCoast" & df$Party == "Democrat",]$Age) # 39.87 < 60.011 - 20

df_neutral <- df[df$Party == "Democrat",]
df_more <- df[df$Location == "EastCoast" & df$Party == "Democrat",]
df_less <- df[df$Location == "WestCoast" & df$Party == "Democrat",]

z_test_from_data(df_neutral, 'Location', 'Age', "EastCoast", "Central") # P-Value = 1
z_test_from_data(df_neutral, 'Location', 'Age', "WestCoast", "Central") # P-Value = 0

################################# PART C

## GIVEN CODE

# RandomElection Code

Net<-unique(df$NetWorth)
Edu <-unique(df$Education) 
Loc<-unique(df$Location)
Party<-unique(df$Party)

Netseq <- sample(Net, 20000, replace = TRUE)
Eduseq<-sample(Edu, 20000, replace = TRUE)
Locseq<-sample(Loc, 20000, replace = TRUE)
Partseq<-sample(Party, 20000, replace = TRUE)
Ageseq<- runif(20000, 18, 100)
colnames(df)
electR<-data.frame(NetWorth=Netseq, Age=Ageseq, Education=Eduseq,Location=Locseq, Party=Partseq)

head(electR)

# Finding by ratios
nrow(df[df$Location == "EastCoast" & df$Party == "Democrat" & df$Age < 90,]) / 
  nrow(df[df$Location == "EastCoast" & df$Party != "Democrat" & df$Age <90,])

nrow(df[df$Location == "EastCoast" & df$Party == "Democrat" & df$Age < 90,]) # 255
nrow(df[df$Location == "EastCoast" & df$Party != "Democrat" & df$Age < 90,]) # 337

new_df <- df[df$Age < 90 & df$Location == "EastCoast",]

z_test_from_data(new_df, 'Party', 'Age', "Republican", "Democrat") # P-Value = 0.335 > 0.05 

###################### PART D

market<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/HomeworkMarket2022.csv")

contingency_table <- table(market$Beer, market$Location)
contingency_table
chisq.test(contingency_table) # 0.7375 # FAIL TO REJECT NULL HYPOTHESIS - WE FAIL to Do Dependency

summary(market)





















































































































