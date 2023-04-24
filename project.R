setwd("C:/Users/jmjsm/OneDrive/Documents/Spring2023/Math363")
getwd()
movies <- read.csv("movies.csv")
class(movies)
movies[28,] 
moviesfiltered <- movies[movies$gross != 0 & movies$score != 0,];
moviesfiltered[7479,13]
nrow(moviesfiltered)
updated_gross <- c()
cpi<-read.csv("cpi_data.csv")
class(cpi)
temp <- cpi[2,1]
temp
class(temp)

#create list of updated gross box office 
for(x in 1:7479){
	j <- 1
	temp1 <- moviesfiltered[x,4]
	temp2 <- cpi[1,1]
	while (temp1 != temp2){
		j = j+1
		temp2 <- cpi[j,1]	
	}
	new_gross <- moviesfiltered[x,13] * (292.655/cpi[j,2])
	updated_gross <- append(updated_gross, new_gross)
}

#dividing grosses by their meadian scores
median_score <- median(moviesfiltered$score)
high_score_movies <- c()
low_score_movies <-c()
for (y in 1:7479){
	if (moviesfiltered[y, 6] > median_score){
		high_score_movies <- append(high_score_movies, updated_gross[y])
	}
	else{
		low_score_movies<- append(low_score_movies, updated_gross[y])
	}	
}
length(high_score_movies)
length(low_score_movies)

t.test(low_score_movies, high_score_movies, alternative = c("less"))


#calulations
m_cov <- cov(updated_gross, moviesfiltered$score)
m_cov
g_var <- var(updated_gross)
s_var <- var(moviesfiltered$score)
g_var
s_var
hist(moviesfiltered$score)
plot(updated_gross, moviesfiltered$score, pch ="*", xlab = "Gross Box Office Sales in USD adjusted to estimated 2022 Cost", ylab = "IMDb Score")
plot(updated_gross, moviesfiltered$score, pch ="*", xlab = "Gross Box Office Sales in USD adjusted to estimated 2022 Cost", ylab = "IMDb Score", log='x')
