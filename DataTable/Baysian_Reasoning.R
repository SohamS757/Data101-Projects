moody<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv')
summary(moody)

#Exercise 1 - 
# Compute the odds that a student failed a class

# Observation: student failed in class
# Belief: the student is freshmen

nf <- nrow(moody[moody$Seniority == "Freshman",])
nf 

n <- nrow(moody)
n

Prior <- nf/n
Prior

PriorOdds <- Prior/(1-Prior)

Truepositive <- round(nrow(moody[moody$Seniority == "Freshman" & moody$Grade == "F",])/nrow(moody[moody$Seniority == "Freshman",]),2)
Truepositive

FalsePositive <- round(nrow(moody[moody$Seniority != "Freshman" & moody$Grade == "F",])/nrow(moody[moody$Seniority != "Freshman",]),2)
FalsePositive

LikelyhoodRatio <- round(Truepositive/FalsePositive, 2)
LikelyhoodRatio

PosteriorOdds <- LikelyhoodRatio * PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds/(1+PosteriorOdds)
Posterior

################################################################################

# Exercise 3 - Contingency Table

# Contingency Table between Seniority and Grade
# Observation : Columns
# Beliefs : Rows

t <- table(moody$Seniority, moody$Grade)
t

PriorProb <- sum(t[1,]) / sum(t [,])
PriorProb

PriorOdds <- PriorProb/(1-PriorProb)
PriorOdds

Truepositive <- t[1,1]/sum(t[1,])
Truepositive

FalsePositive <- sum(t[2:4, 1]) / sum(t[2:4, ])
FalsePositive

Likelihoodratio <- Truepositive/FalsePositive
Likelihoodratio

PosteriorOdds <- Likelihoodratio *PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds / (1+PosteriorOdds)
Posterior

################################################################################

# Exercise 4 - Reverse the Belief and Observations


t <- table(moody$Seniority, moody$Grade)
t

PriorProb <- sum(t[,1]) / sum(t [,])
PriorProb

PriorOdds <- PriorProb/(1-PriorProb)
PriorOdds

Truepositive <- t[1,1]/sum(t[,1])
Truepositive

FalsePositive <- sum(t[1, 2:5]) / sum(t[ ,2:5 ])
FalsePositive

Likelihoodratio <- Truepositive/FalsePositive
Likelihoodratio

PosteriorOdds <- Likelihoodratio *PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds / (1+PosteriorOdds)
Posterior

################################################################################








