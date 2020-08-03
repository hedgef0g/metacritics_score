# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("dbscan")
# install.packages("zoo")
# install.packages("string")

library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(dbscan)
library(zoo)
library(stringr)

# remove <- c("llc", "LLC", "L.L.C", 
#             "ltd", "Ltd", "LTD", 
#             "inc", "Inc", "INC",
#             "[.]$")

clean_base <- as_tibble(complete_base) %>% 
  mutate(Platform = as_factor(Platform),
         ReleaseDate = mdy(ReleaseDate),
         UserScore = as.numeric(UserScore),
         UserReviews = as.numeric(UserReviews),
         MetaScore = as.numeric(MetaScore),
         CriticReviews = as.numeric(CriticReviews))

# i = 1
# 
# while(i < nrow(base_df)) {
#   base_df$Genres[i] <- paste(unique(unlist(str_split(trimws(base_df$Genres[i]), ","))), "", collapse = ",")
#   i = i + 1
# }

# number_of_genres <- max(lengths(str_split(base_df$Genres, ", ")))

# clean_base <- base_df %>%
#   separate(Genres, c(rep(paste("Genre", 1:number_of_genres))), sep = ",")

# rm(list = c("number_of_genres", "i"))