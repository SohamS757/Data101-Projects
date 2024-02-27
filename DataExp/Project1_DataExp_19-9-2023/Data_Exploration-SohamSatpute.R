# Link to DATASET

setwd("D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/Projects/Project1_DataExp_19-9-2023")
data <- read.csv("Airline.csv")
summary(data)

# The mean age of flight attendees is 45.3 even though the life expectancy average is only about 72 years
mean_age <- mean(data$Age)
mean_age

# The flight status is evenly distributed instead of mostly being on-time
table1 <- table(data$Flight.Status)
barplot(table1,
        col = "green", main = "Flight Status Frequencyy",
        xlab = "Flight Status", ylab = "No. of Flights")

# United States has the most Flight Frequency of 22104 though being quite less populated
table2 <- table(data$Country.Name)
table2
barplot(table2,
        col = "blue", main = "Flight Frequency",
        xlab = "Country", ylab = "Number of Flights")

# People whose names begin with Aa contain about 5% of the total population (Statistically should be 0.14793%)
# Had to use google search to come up with desired syntax here
table3 <- table(data$First.Name)
table3
barplot(table3,
        col = "red", main = "First Name Frequency",
        xlab = "Names", ylab = "Number of Names")
name_freq_a <- table(grepl("^Aa", data$First.Name, ignore.case = TRUE))
print(name_freq_a)

name_freq_s <- table(grepl("^S", data$First.Name, ignore.case = TRUE))
print(name_freq_s)

# Out of all the names the most popular is name Abigail with the frequency close to 3% (3245) of total population
name_freq_s <- table(grepl("^Abigail", data$First.Name, ignore.case = TRUE))
print(name_freq_s)

# The maximum age of a person who used air travel is 102 which is quite unexpected  
max_age <- max(data$Age)
max_age

# It is surprising to see that the frequency of flight takers are evenly distributed instead of normally distributed
table4 <- table(data$Age)
table4
barplot(table4,
        col = "purple", main = "Flyers by Age",
        xlab = "Age", ylab = "Frequency")

# Its quite unexpected to see only male and female genders in flyers, with this evolving world I hoped the dataset contained of other genders (created in 2022)
# Not a good finding but will make work as a point
# I expected more than 2 genders
table5 <- table(data$Gender)
table5
barplot(table5,
        col = "yellow", main = "Flyers by Gender",
        xlab = "Genders", ylab = "Frequency")

# North America has the most amount of airplanes whereas South America being the least - kinda ironic
# However, i did expect Asia to be the most die to having more countries and population
table6 <- table(data$Continents)
table6
barplot(table6,
        col = "pink", main = "Flyers by Continents",
        xlab = "Continents", ylab = "Frequency")

# China has a WHOOPING 18317 Citizens flying planes even though most flights are from USA
table7 <- table(data$Nationality)
table7
barplot(table7,
        col = "black", main = "Flyers by Nationality",
        xlab = "Nationality", ylab = "Frequency")


