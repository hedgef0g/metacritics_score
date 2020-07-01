# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("dbscan")
# install.packages("zoo")

library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(dbscan)
library(zoo)

base_df <- as_tibble(complete_base)

base_df <- base_df %>% 
  mutate(Platform = as_factor(Platform),
         ReleaseDate = mdy(ReleaseDate),
         UserScore = as.numeric(UserScore),
         UserReviews = as.numeric(UserReviews),
         MetaScore = as.numeric(MetaScore),
         CriticReviews = as.numeric(CriticReviews),
         Genres = lapply(str_split(Genres, ", "), unique))
         
glimpse(base_df)
base_df

base_df %>%
  filter(!is.na(UserScore) & !is.na(MetaScore) & year(ReleaseDate) > 2000) %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  arrange(ReleaseDate) %>%
  group_by(ReleaseDate) %>%
  summarize(corr = cor(UserScore, MetaScore), diff = mean(abs(UserScore - MetaScore/10))) %>%
  ggplot(aes(x = ReleaseDate, y = diff)) +
  geom_col() +
  geom_line(aes(y = corr))

base_df %>%
  filter(!is.na(UserScore) & !is.na(MetaScore)) %>%
  summarize(diff = (UserScore - MetaScore/10)) %>%
  ggplot(aes(x = diff)) +
  geom_histogram()

lev <- base_df %>%
  filter(!is.na(UserScore) & !is.na(MetaScore)) %>%
  summarize(diff = abs(UserScore - MetaScore/10)) %>%
  summarize(3 * sd(diff)) %>%
  as.numeric()

controversial <- base_df %>%
  filter(!is.na(UserScore) & !is.na(MetaScore) & year(ReleaseDate) >= 2000) %>%
  mutate(diff = abs(UserScore - MetaScore/10)) %>%
  filter(diff > lev)

main_sample <- base_df %>%
  filter(!is.na(UserScore) & !is.na(MetaScore) & year(ReleaseDate) >= 2000) %>%
  mutate(diff = abs(UserScore - MetaScore/10)) %>%
  filter(diff <= lev) %>%
  summary()

base_df %>%
  filter(Platform == "PC") %>%
  count()
