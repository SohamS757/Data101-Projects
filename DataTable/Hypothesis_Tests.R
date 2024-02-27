# install.packages("devtools")
# devtools::install_github("janish-parikh/ZTest")
# library(devtools)
# library(HypothesisTesting)
  
movies<-read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv")

mean(movies[movies$country == "Netherlands"]$imdb_score)
mean(movies[movies$country == "Chile"]$imdb_score)

# Permulation_test(df, "CAT", "NUM", N, "v1", "v2")
# df = dataframe, CAT = Categorical, NUM = Numerical N=no.of permutations, v1 v2 are variable CAT 
permutation_test(movies, 'country', 'imdb_score',10000, "Chile", "Netherlands")

# z_test(df, "CAT", "NUM", "v1", "v2")
z_test_from_data(movies, 'country', 'imdb_score', "Chile", "Netherlands")
# Always the same value (p-value)
# If the p-value is less than 0.05 then we reject null hypothesis

## Example 2 
df <- movies[movies$genre == "Action", ]

mean(df[df$country == "USA", ]$imdb_score)
mean(df[df$country == "UK", ]$imdb_score)
permutation_test(df, 'country', 'imdb_score',10000, "USA", "UK")
z_test_from_data(df, 'country', 'imdb_score', "USA", "UK")


###########################################################################################

#Party dataset
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")

df <- party[party$DJ == "Mski",]

mean(df[df$Day == "Saturday", ]$Ticket)
mean(df[df$Day == "Friday", ]$Ticket)
permutation_test(df, 'Day', 'Ticket',10000, "Friday", "Saturday")
z_test_from_data(df, 'Day', 'Ticket', "Friday", "Saturday")
# When p-value is close to 1, reverse hypothesis


df <- party[party$Music == "Jazz",]
mean(df[df$DJ == "Rohit", ]$Rating)
mean(df[df$DJ == "Mski", ]$Rating)
permutation_test(df, 'DJ', 'Rating',10000, "Rohit", "Mski")
z_test_from_data(df, 'DJ', 'Rating', "Rohit", "Mski")                           


















