

moody <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")
moody

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
days

# Ctrl + Enter for Line Execution

year <- 2008:2023
year

moody[2,]
moody[1:5, c(2:4)]

grades <- table(moody$GPA, moody$Major)
grades

sd(moody$Score)
moody[moody$Score >90 & moody$Grade == 'A' & moody$Major =="CS" & moody$Seniority == "Junior", ]  

colnames(moody)
moody3 <- subset(moody, select= -c(3))

moody3

moody1 <- subset(moody, select = c(2:4), Major=="CS")
dim(moody1)
moody1

tapply(moody[moody$Seniority == 'Junior',]$Score, moody[moody$Seniority == 'Junior',]$Grade,mean)
summary(moody)
unique(moody$Major)

max(moody[moody$Grade=='D',]$Score)

moody[moody$Score == "0",]

tapply(moody[moody$Grade=='A',]$Score, moody[moody$Grade=='A',]$Major, min)

moody$ScoreIntervals<-cut(moody$Score,breaks=c(0,60,90,95,100),labels=c("Low","Medium",'Good', "Excellent"))
table(moody$ScoreIntervals)

table(moody$Major, moody$Grade)