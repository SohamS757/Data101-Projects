movies<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv')

# Summary of Movies
summary(movies)

# Extracting Columns
table(movies$genre, movies$Budget)


tapply( movies$imdb_score, movies$genre,mean)

#Bar Plot

genre_country <- table(movies[movies$imdb_score > 6.5,]$genre)
genre_country

barplot(genre_country)

gross_counts <- table(movies[movies$genre == "Family",]$Gross)
gross_counts

barplot(gross_counts)
paints <- ("red","blue","green","yellow","purple")
mosaicplot(movies$content~movies$genre, xlab="Genre", ylab="Content",  main = "The content vs genre mosaic plot", colors=paints)

# Scatterplt

x = sample(1:100, 50)
y = sample(1:100, 50)

plot(x, y)




