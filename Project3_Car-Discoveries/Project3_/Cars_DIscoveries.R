# Set the directory location and extract data
setwd("D:/RUTGERS_UNIVERSITY/Academic Fall 2023/Data101/Projects/Project3_")
data <- read.csv("Cars2023.csv")

# Extract the basic values and information about data 
head(data)
summary(data)

# Extract Unique Categorical Values in column of data
unique(data$Dealership)
unique(data$Season)
unique(data$Car)
unique(data$Buyer)

real_mean <- mean(data$Buyer_Income)
real_mean

###################### Query 1 Work ############################################
mean_above_3000 <- mean(data[data$Dealership == "Chicago" & data$Car == "Chevrolet", ]$Buyer_Income)
cat("3000- Mean (Chicago + Chevrolet):" , mean_above_3000)
diff_mean_above <- mean_above_3000 - real_mean
cat("Difference:" , diff_mean_above) # 5525.862

###################### Query 2 Work ############################################
mean_below_3000 <- mean(data[data$Dealership == "NYC" & data$Car == "GMC", ]$Buyer_Income)
cat("3000- Mean (NYC + GMC):" , mean_below_3000)
diff_mean_below <- mean_below_3000 - real_mean
cat("Difference:" , diff_mean_below) # -3178.895

# ---------------------------------------------------------------------------- #
# Cross Valid (Splitting the data into two) NO 1

Cars1 <- data[1:25000, ]
Cars2 <- data[25000:50000, ]

# Cars1
mean_above_3000_Cars1 <- mean(Cars1[Cars1$Dealership == "Chicago" & Cars1$Car == "Chevrolet", ]$Buyer_Income)
cat("3000- Mean (Chicago + Chevrolet):" , mean_above_3000_Cars1)
diff_mean_above_Cars1 <- mean_above_3000_Cars1 - real_mean
cat("Difference:" , diff_mean_above_Cars1) # 8930.507

mean_below_3000_Cars1 <- mean(Cars1[Cars1$Dealership == "NYC" & Cars1$Car == "GMC", ]$Buyer_Income)
cat("3000- Mean (NYC + GMC):" , mean_below_3000_Cars1)
diff_mean_below_Cars1 <- mean_below_3000_Cars1 - real_mean
cat("Difference:" , diff_mean_below_Cars1) # -537.6488

# Cars2
mean_above_3000_Cars2 <- mean(Cars2[Cars2$Dealership == "Chicago" & Cars2$Car == "Chevrolet", ]$Buyer_Income)
cat("3000- Mean (Chicago + Chevrolet):" , mean_above_3000_Cars2)
diff_mean_above_Cars2 <- mean_above_3000_Cars2 - real_mean
cat("Difference:" , diff_mean_above_Cars2) # 2042.768

mean_below_3000_Cars2 <- mean(Cars2[Cars2$Dealership == "NYC" & Cars2$Car == "GMC", ]$Buyer_Income)
cat("3000- Mean (NYC + GMC):" , mean_below_3000_Cars2)
diff_mean_below_Cars2 <- mean_below_3000_Cars2 - real_mean
cat("Difference:" , diff_mean_below_Cars2) # -5683.738

# ---------------------------------------------------------------------------- #
# Cross Valid (Splitting the data into two) NO 2
# Not to be Graded - just for testing

Cars1 <- data[1:12500 & 37500:50000, ]
Cars2 <- data[25000:37500 & 12500:25000, ]

# Cars1
mean_above_3000_Cars1 <- mean(Cars1[Cars1$Dealership == "Chicago" & Cars1$Car == "Chevrolet", ]$Buyer_Income)
cat("3000- Mean (Chicago + Chevrolet):" , mean_above_3000_Cars1)
diff_mean_above_Cars1 <- mean_above_3000_Cars1 - real_mean
cat("Difference:" , diff_mean_above_Cars1) 

mean_below_3000_Cars1 <- mean(Cars1[Cars1$Dealership == "NYC" & Cars1$Car == "GMC", ]$Buyer_Income)
cat("3000- Mean (NYC + GMC):" , mean_below_3000_Cars1)
diff_mean_below_Cars1 <- mean_below_3000_Cars1 - real_mean
cat("Difference:" , diff_mean_below_Cars1) 

# Cars2
mean_above_3000_Cars2 <- mean(Cars2[Cars2$Dealership == "Chicago" & Cars2$Car == "Chevrolet", ]$Buyer_Income)
cat("3000- Mean (Chicago + Chevrolet):" , mean_above_3000_Cars2)
diff_mean_above_Cars2 <- mean_above_3000_Cars2 - real_mean
cat("Difference:" , diff_mean_above_Cars2) 

mean_below_3000_Cars2 <- mean(Cars2[Cars2$Dealership == "NYC" & Cars2$Car == "GMC", ]$Buyer_Income)
cat("3000- Mean (NYC + GMC):" , mean_below_3000_Cars2)
diff_mean_below_Cars2 <- mean_below_3000_Cars2 - real_mean
cat("Difference:" , diff_mean_below_Cars2) 









