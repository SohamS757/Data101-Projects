# Read in the data
Cars <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv")

nrow(Cars)
summary(Cars)
unique(Cars$Dealership)
unique(Cars$Car)
unique(Cars$Season)
unique(Cars$Buyer)
table(Cars$Dealership)
table(Cars$Car)
table(Cars$Season)
table(Cars$Buyer)
tapply(Cars$Buyer_Age, Cars$Car, mean)
tapply(Cars$Buyer_Age, Cars$Dealership, mean)
tapply(Cars$Buyer_Age, Cars$Season, mean)

# Sample of 5000
v <- sample(1:nrow(Cars))[1:5000]
SampleCars <- Cars[v, ]
SampleCars[1:10,]

tapply(SampleCars[SampleCars$Dealership=='Chicago',]$Buyer_Age, SampleCars[SampleCars$Dealership=='Chicago',]$Car, mean)

# Playingsimulation

Elections <-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/ElectionsID.csv")
colnames(Elections)

v <- sample(1:nrow(Elections))
Elections<-Elections[v,]
SampleE<-Elections[1:100,]
table(SampleE$Party)
