if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(dversionata.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")





library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(tidyverse)
library(caret)



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = 
                                            as.numeric(movieId), 
                                          title = as.character(title),
                                          genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation <- validation %>% select(-rating)
# Ratings will go into the CSV submission file below:
write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),
          "submission.csv", na = "", row.names=FALSE)
rm(dl, ratings, movies, test_index, temp, movielens, removed)







if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)
ratings_freqency <- edx %>% 
  group_by(rating) %>% 
  count() %>% 
  rename("Rating"=rating, "Frequency"=n) %>% 
  mutate("Of total"=paste(round(100*Frequency/nrow(edx),2),"%",sep=""))
kable(ratings_freqency, align = rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1,bold=T,border_right = T)


length(unique(edx$movieId))


length(unique(edx$userId))




genre_analysis <- edx %>% mutate(Is.Drama = str_detect(genres, "Drama"),
                                 Is.Comedy = str_detect(genres,"Comedy"),
                                 Is.Thriller = str_detect(genres, "Thriller"),
                                 Is.Romance = str_detect(genres, "Romance"))
genre_analysis <- data_frame("Genre"=c("Drama", "Comedy", "Thriller", "Romance"),
                             "Ratings included"=c(sum(genre_analysis$Is.Drama),
                                                  sum(genre_analysis$Is.Comedy),
                                                  sum(genre_analysis$Is.Thriller), 
                                                  sum(genre_analysis$Is.Romance)))


kable(genre_analysis, align = rep("c",2)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1,bold=T,border_right = T)