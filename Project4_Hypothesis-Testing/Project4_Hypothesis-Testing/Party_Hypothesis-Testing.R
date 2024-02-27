install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)



# Reading the party data into df
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")
summary(party)
head(party)

# Finding Unique values of specific columns
unique(party$Day)                   # Thursday Saturday Friday
unique(party$DJ)                    # Alex Ania Carol Rohit Maki Blue
unique(party$Music)                 # Jazz Techno Rock Salsa HipHop


# # # # # # # # # # # # # # # # Part A # # # # # # # # # # # # # # # # # #
"The most Promosing ALternative Hypothesis"

# Q1

# Rock Parties have higher attendance than Salsa parties on Thursday 
df <- party[party$Day == "Thursday", ]
mean(df[df$Music == "Rock",]$Attendance)
mean(df[df$Music == "Salsa",]$Attendance)
permutation_test(df, 'Music', 'Attendance',10000, 'Salsa', 'Rock')
z_test_from_data(df, 'Music', 'Attendance', 'Salsa','Rock') # 0.0898101

# Q2

# Jazz Parties have higher attendance than Techno Parties with Rohit DJ 
df <- party[party$DJ == "Rohit", ]
mean(df[df$Music == "Techno",]$Attendance)
mean(df[df$Music == "Jazz",]$Attendance)
permutation_test(df, 'Music', 'Attendance',10000, 'Techno', 'Jazz')
z_test_from_data(df, 'Music', 'Attendance', 'Techno','Jazz') # 0.0858 P-value

#Q3

# Techno Parties have higher attendance than Jazz Parties with Alex DJ 
df <- party[party$DJ == "Alex", ]
mean(df[df$Music == "Techno",]$Attendance)
mean(df[df$Music == "Jazz",]$Attendance)
permutation_test(df, 'Music', 'Attendance',10000, 'Jazz', 'Techno')
z_test_from_data(df, 'Music', 'Attendance', 'Jazz','Techno') # 0.0297 P-value

# # # # # # # # # # # # # # # # Part B # # # # # # # # # # # # # # # # # #

#PartA - Very small P-value
# Null :       Jazz Parties have the same attendance as Techno Parties with Alex DJ on Fridays 
# Alternative: Jazz Parties have higher attendance than Techno Parties with Alex DJ on Fridays 

df <- party[party$DJ == "Alex" & party$Day == "Friday", ]
mean(df[df$Music == "Techno",]$Attendance) # 61.826
mean(df[df$Music == "Jazz",]$Attendance) # 47.028
permutation_test(df, 'Music', 'Attendance',10000, 'Jazz', 'Techno')
z_test_from_data(df, 'Music', 'Attendance', 'Jazz','Techno') # 0.0022 P-value

#PartB - Small P-value
# Null       : Jazz Parties have the same attendance as Techno Parties with Alex DJ 
# Alternative: Jazz Parties have higher attendance than Techno Parties with Alex DJ 

df <- party[party$DJ == "Alex", ]
mean(df[df$Music == "Techno",]$Attendance)
mean(df[df$Music == "Jazz",]$Attendance)
permutation_test(df, 'Music', 'Attendance',10000, 'Jazz', 'Techno')
z_test_from_data(df, 'Music', 'Attendance', 'Jazz','Techno') # 0.03 P-value

#PartC - Large P-value
# Null       : Rock Parties have the same attendance as Salsa parties on Thursday 
# Alternative: Rock Parties have higher attendance than Salsa parties on Thursday 

df <- party[party$Day == "Thursday", ]
mean(df[df$Music == "Rock",]$Attendance)
mean(df[df$Music == "Salsa",]$Attendance)
permutation_test(df, 'Music', 'Attendance',10000, 'Salsa', 'Rock')
z_test_from_data(df, 'Music', 'Attendance', 'Salsa','Rock') # 0.0898101

