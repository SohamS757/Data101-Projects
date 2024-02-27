# Vector Creation
color <- c("Red","Blue","Yello","Green")
color

# Vector with Numerical Sequence
year <- 2006:2023
year

# Import Data from Github repository(Cloud)
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")
head(moody)

# Return Specific rowns and columns moody(row : row-end, column : column-end)
print(moody[976:1000, ])

# Tables for Distribution

  # Single Distribution
  grades <- table(moody$Major)
  grades
  
  # Joint Distribution 
  table(moody$Major, moody$GPA)
  
# Min / Mean / Max FUnction
max(moody$Score)
min(moody$Score)
mean(moody$Score)

sd(moody$Score)

# Subset Extraction
moody[moody$Major != "CS" & moody$GPA == 4.0 & moody$Grade == "A" & moody$Score > 90, ]
summary(moody[moody$Major == "Psycology", ])

# Using tapply(vector, index, function)
tapply(moody[moody$Seniority == 'Junior',]$Score, moody[moody$Seniority == 'Junior',]$Grade,mean)

# Let's look at a 2 attribute scatter plot.
# moody<-read.csv("../files/dataset/moody2020b.csv") #static Load
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2020b.csv") #web load
head(moody)
plot(moody$participation,moody$score,ylab="score",xlab="participation",main=" Participation vs Score",col="red")
























































