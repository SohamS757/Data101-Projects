install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)

# Reading the movies data into df
movies<-read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv")
summary(movies)
head(movies)

unique(movies$country)
unique(movies$content)
unique(movies$Budget)
unique(movies$Gross)
unique(movies$genre)

summary(movies$Gross)

#Calculating different values present in each categorical variable

country_count <- length(unique(movies$country))
country_count

content_count <- length(unique(movies$content))
content_count

gross_count <- length(unique(movies$Gross))
gross_count

budget_count <- length(unique(movies$Budget))
budget_count

genre_count <- length(unique(movies$genre))
genre_count

#Calculate the significance level for multiple hypothesis using Nonferrous coefficient
#  IF there are N items, them m = N : m = N(N-1)/2


m_country <- country_count * (country_count - 1)/2
country_significance <- 0.05/m_country

m_content <- content_count * (content_count - 1)/2
content_significance <- 0.05/m_content
content_significance

m_gross <- gross_count * (gross_count - 1)/2
gross_significance <- 0.05/m_country

m_budget <- budget_count * (budget_count - 1)/2
budget_significance <- 0.05/m_country

m_genre <- genre_count& (genre_count - 1)/2
genre_significance <- 0.05/m_genre


permutation_test(movies, "country", "imdb_score", 10000, "Chile", "Netherlands")
permutation_test(movies, "country", "imdb_score", 10000, "Taiwan", "Brazil")
permutation_test(movies, "country", "imdb_score", 10000, "Italy", "India")
permutation_test(movies, "country", "imdb_score", 10000, "Spain", "Sweden")
permutation_test(movies, "country", "imdb_score", 10000, "Hungary", "USA")

#If the p-value is below the significance level of 0.00333, we have enough evidences to reject the null hypothesis
#We fail to reject the Hypothesis with p-value 0.0000377

tapply(movies$imdb_score, movies$genre, mean)
tapply(movies$imdb_score, movies$Gross, mean)
tapply(movies$imdb_score, movies$Budget, mean)

permutation_test(movies, "content", "imdb_score", 10000, "PG-13", "R")



#Party Dataset
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")

music_count <- length(unique(party$Music))
day_count <- length(unique(party$Day))
DJ_count <- length(unique(party$DJ))

m_music <- music_count * (music_count - 1)/2
music_significance <- 0.05/m_music
music_significance

m_day <- day_count * (day_count - 1)/2
day_significance <- 0.05/m_day
day_significance

m_DJ <- DJ_count * (DJ_count - 1)/2
DJ_significance <- 0.05/m_DJ
DJ_significance

# Tapply to get the mean
tapply(party$Attendance, party$DJ, mean)

permutation_test(party, 'DJ', 'Attendance', 10000, 'Alex','Ania')
permutation_test(party, 'DJ', 'Attendance', 10000, 'Alex','Blue')
permutation_test(party, 'DJ', 'Attendance', 10000, 'Rohit','Mski')
permutation_test(party, 'DJ', 'Attendance', 10000, 'Carol','Blue')


tapply(party$Attendance, party$Music, mean)
tapply(party$Attendance, party$Day, mean)

permutation_test(party, 'Music', 'Attendance', 10000, 'Salsa','Techno')

permutation_test(party, 'Day', 'Attendance', 10000, 'Thursday','Friday')

















































































