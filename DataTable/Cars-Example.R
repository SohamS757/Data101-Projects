cars <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv")
head(cars)
summary(cars)

# False Discoveries using narrow Queries

Cars1 <- cars[1:25000, ]
Cars2 <- cars[25000:50000, ]

m1 <- mean(Cars1[Cars1$Dealership == "Chicago" & Cars1$Car == "Ford",]$Buyer_Age)
m1

m2 <- mean(Cars2[Cars2$Dealership == "Chicago" & Cars2$Car == "Ford",]$Buyer_Age)
m2

m_main <- mean(cars[cars$Dealership == "Chicago" & cars$Car == "Ford",]$Buyer_Age)
m_main

round(table(Cars1[Cars1$Dealership == "Chicago" & Cars1$Buyer_Age<22,]$Car) / 
        nrow(Cars1[Cars1$Dealership == "Chicago" & Cars1$Buyer_Age<22,]), 4)
      
round(table(Cars2[Cars2$Dealership == "Chicago" & Cars2$Buyer_Age<22,]$Car) / 
        nrow(Cars1[Cars2$Dealership == "Chicago" & Cars2$Buyer_Age<22,]), 4)

unique(cars)

# Checking the same inference from Austin

round(table(Cars1[Cars1$Dealership == "Austin" & Cars1$Buyer_Age<22,]$Car) / 
        nrow(Cars1[Cars1$Dealership == "Austin" & Cars1$Buyer_Age<22,]), 4)

round(table(Cars2[Cars2$Dealership == "Austin" & Cars2$Buyer_Age<22,]$Car) / 
        nrow(Cars2[Cars2$Dealership == "Austin" & Cars2$Buyer_Age<22,]), 4)


# San Antonio / Female / Age > 45
round(table(Cars2[Cars2$Dealership == "San Antonio" & Cars2$Buyer_Age>45 & Cars2$Buyer == "Female",]$Car) / 
        nrow(Cars2[Cars2$Dealership == "San Antonio" & Cars2$Buyer_Age>45 & Cars2$Buyer == "Female",]), 4)


round(table(Cars1[Cars1$Dealership == "San Antonio" & Cars1$Buyer_Age>45 & Cars1$Buyer == "Female",]$Car) / 
        nrow(Cars1[Cars1$Dealership == "San Antonio" & Cars1$Buyer_Age>45 & Cars1$Buyer == "Female",]), 4)

# Average Age Distribution grouped by all care makes in summer chicago

tapply(Cars1[Cars1$Dealership == "Chicago" & Cars1$Season == "Summer",]$Buyer_Age,
       Cars1[Cars1$Dealership == "Chicago" & Cars1$Season == "Summer",]$Car, mean) 

tapply(Cars2[Cars2$Dealership == "Chicago" & Cars2$Season == "Summer",]$Buyer_Age,
       Cars2[Cars2$Dealership == "Chicago" & Cars2$Season == "Summer",]$Car, mean) 

# Average AGe Dustribution grouped by all care makes in summer Austin 
tapply(Cars1[Cars1$Dealership == "Austin" & Cars1$Season == "Summer",]$Buyer_Age,
       Cars1[Cars1$Dealership == "Austin" & Cars1$Season == "Summer",]$Car, mean) 

tapply(Cars2[Cars2$Dealership == "Austin" & Cars2$Season == "Summer",]$Buyer_Age,
       Cars2[Cars2$Dealership == "Austin" & Cars2$Season == "Summer",]$Car, mean) 



tapply(Cars2[Cars2$Dealership == "NYC" & Cars2$Season == "Fall",]$Buyer_Age,
       Cars2[Cars2$Dealership == "NYC" & Cars2$Season == "Fall",]$Car, mean) 

unique(Cars1$Dealership)
#$ Age of female bias over age of 30 buying Honda 

round(table(Cars1[Cars1$Dealership == "LA" & Cars1$Buyer_Age>30 & Cars1$Buyer == "Female" & Cars1$Car == "Honda", ]$Season) /
      nrow(Cars1[Cars1$Dealership == "LA" & Cars1$Buyer_Age>30 & Cars1$Buyer == "Female" & Cars1$Car == "Honda", ]) ,4 )
      
round(table(Cars2[Cars2$Dealership == "LA" & Cars2$Buyer_Age>30 & Cars2$Buyer == "Female" & Cars2$Car == "Honda", ]$Season) /
        nrow(Cars2[Cars2$Dealership == "LA" & Cars2$Buyer_Age>30 & Cars2$Buyer == "Female" & Cars2$Car == "Honda", ]) ,4 )

# Small samples are susceptible of producing more extraneous results compared to larger samples. 
      
      
      