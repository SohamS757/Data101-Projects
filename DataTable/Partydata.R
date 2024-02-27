setwd("D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/DataTable")
data <- read.csv("PartyR.csv")

# Shows Unique values in a column
str(data)

summary(data)
head(data)

table(data$Day, data$)

# Shows the unique values in each column 
unique(data$Day)
unique(data$DJ)
unique(data$Music)

# Shows Summary of specific column only
summary(data$Attendance)

# Check value distribution with table()
table(data$Day)
table(data$DJ)
table(data$Music)

# Comparing two columns  - one cat and one numerical
tapply(data$Attendance, data$Day, mean)
tapply(data$Attendance, data$DJ, mean)
tapply(data$Attendance, data$Music, mean)

# Selecting Saturday to dig into information
partySat <- data[data$Day == "Saturday", ]
tapply(partySat$Attendance, partySat$DJ, mean)

partySat <- data[data$Day == "Saturday" & data$DJ == "Blue",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Saturday" & data$DJ == "Monique",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Saturday" & data$DJ == "Brejcha",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Saturday" & data$DJ == "Mski",]
tapply(partySat$Attendance, partySat$Music, mean)

partySat <- data[data$Day == "Saturday" & data$Music == "Jazz",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Saturday" & data$Music == "Rock",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Saturday" & data$Music == "Techno",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Saturday" & data$Music == "HipHop",]
tapply(partySat$Attendance, partySat$DJ, mean)

# Checking Friday the same way

partySat <- data[data$Day == "Friday" & data$DJ == "Blue",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Friday" & data$DJ == "Monique",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Friday" & data$DJ == "Brejcha",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Friday" & data$DJ == "Mski",]
tapply(partySat$Attendance, partySat$Music, mean)

partySat <- data[data$Day == "Friday" & data$Music == "Jazz",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Friday" & data$Music == "Rock",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Friday" & data$Music == "Techno",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Friday" & data$Music == "HipHop",]
tapply(partySat$Attendance, partySat$DJ, mean)

# Checking Thursday the same way

partySat <- data[data$Day == "Thursday" & data$DJ == "Blue",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Thursday" & data$DJ == "Monique",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Thursday" & data$DJ == "Brejcha",]
tapply(partySat$Attendance, partySat$Music, mean)
partySat <- data[data$Day == "Thursday" & data$DJ == "Mski",]
tapply(partySat$Attendance, partySat$Music, mean)

partySat <- data[data$Day == "Thursday" & data$Music == "Jazz",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Thursday" & data$Music == "Rock",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Thursday" & data$Music == "Techno",]
tapply(partySat$Attendance, partySat$DJ, mean)
partySat <- data[data$Day == "Thursday" & data$Music == "HipHop",]
tapply(partySat$Attendance, partySat$DJ, mean)

# Boxplot Plotting

partyS <- data[data$DJ == "Mski" & data$Music == "HipHop",]
tapply(partyS$Attendance, partyS$Day, mean)
boxplot(partyS$Attendance~partyS$Day)

# Mean Function - ANSWERS
mean(data[data$Day == "Saturday" & data$Music == "HipHop" & data$DJ == "Mski", ]$Attendance)
mean(data[data$Day == "Saturday" & data$Music == "HipHop" & data$DJ == "Blue", ]$Attendance)
mean(data[data$Day == "Friday" & data$Music == "Jazz" & data$DJ == "Blue", ]$Attendance)

# COnfirm and Visualize Boxplots
boxplot(data[data$Day == "Saturday" & data$Music == "HipHop" & data$DJ == "Mski", ]$Attendance)

# Freq anolomy detection
table(data$Day)
table(data$DJ)
table(data$Music)

party <- data[data$DJ == "Mski",]
table(party$Day)
table(party$Music)

party <- data[data$DJ == "Blue",]
table(party$Day, party$Music)

party <- data[data$Music == "HipHop",]
table(party$Day, party$DJ)
