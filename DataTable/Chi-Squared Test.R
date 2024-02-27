
# Reading the data 
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")
summary(moody)

# Chi Squared Test
# Null Hypothesis: There is no relation between 2 variables
# Alternative Hypothesis: There is a relation (dependency) between 2 variables

# Degree of Freedom = (row-1) * (col-1)

contingency_table <- table(moody$Major, moody$Grade)
contingency_table
chisq.test(contingency_table) # 2.22e-8 < 0.05 REJECT NULL HYPOTHESIS

contingency_table2 <- table(moody$Seniority, moody$Grade)
contingency_table2
chisq.test(contingency_table2) # 0.001829 < 0.05 REJECT NULL HYPOTHESIS

contingency_table3 <- table(moody$Seniority, moody$Major)
contingency_table3
chisq.test(contingency_table3)


# ROR: Reporting Odds Ratio

a <- 20
b <- 980
c <- 200
d <- 800

ROR <- (a/b) / (c/d)
eln_ROR <- log(ROR)

# Calculate the Lower and Upper Bonds
lower_CI <- exp(eln_ROR - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
upper_CI <- exp(eln_ROR + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))

lower_CI
upper_CI

# Since lower_CI < 1; we conclude that there is no relation between cat(variables)






































































