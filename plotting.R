#install.packages("arules")

library(tidyverse)
library(lubridate)

meta_base <- read_tsv("metacritics_score.csv")
meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate > 1995) %>%
  group_by(ReleaseDate) %>%
  summarise(correlation = cor(UsersScore, CriticsScore)) %>%
  mutate(meancor = mean(correlation), sdcor = sd(correlation)) %>%
  ggplot(aes(x = ReleaseDate, y = correlation, fill = correlation)) +
  geom_col() +
  scale_x_discrete(breaks = seq(1996, 2020, by = 1)) +
  ylim(0, 1) +
  xlim(1996, 2020) +
  theme_classic() +
  labs(x = "Год выхода игры", y = "Корреляция между оценками критиков и игроков",
       title ="Насколько солидарны игровые журналисты и игроки?",
       subtitle = "Корреляция между оценками критиков и игроков (ближе к 1 = сильнее)",
       caption = "Источник данных: metacritic.com, 24.06.2020") +
  theme(legend.position="none") 

current_gen <- c("PC", "Xbox One", "Switch", "PlayStation 4")

meta_base_cor <- meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(Platform %in% current_gen, ReleaseDate >= 2013) %>%
  group_by(Platform) %>%
  summarise(correlation = str_c("Корреляция = ", round(cor(UsersScore, CriticsScore),3)))

meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(Platform %in% current_gen, ReleaseDate >= 2013) %>%
  ggplot(aes(x = UsersScore, y = CriticsScore, alpha = 0.1, color = Platform)) +
  geom_point() + 
  facet_wrap(Platform ~ .) +
  geom_text(x = 7.5, y = 20, aes(label = correlation), data = meta_base_cor, color = "black") +
  geom_smooth(color = "Light Blue", se = FALSE) +
  theme(legend.position="none") +
  labs(x = "Пользовательская оценка", y = "Оценка критиков",
       title ="Связь между оценками критиков и игроков",
       subtitle = "В разрезе current gen платформ; игры вышедшие с 2013 года",
       caption = "Источник данных: metacritic.com, 24.06.2020")

ggplot(data = meta_base, aes(x = UsersScore)) +
  geom_histogram(aes(y = stat(count)/sum(count)))+
  scale_y_continuous(labels = scales::percent)
ggplot(data = meta_base, aes(x = CriticsScore)) +
  geom_histogram(aes(y = stat(count)/sum(count)))+
  scale_y_continuous(labels = scales::percent)

summary(meta_base)
meta_base %>%
  filter(Platform != "Stadia") %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2000) %>%
  ggplot(aes(x = UsersScore)) +
    geom_boxplot() +
    facet_wrap(ReleaseDate ~ .)

meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate == 2020) %>%
  summary()

meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2000) %>%
  group_by(ReleaseDate) %>%
  summarize(CriticsScore = mean(CriticsScore)) %>%
  mutate(sdCs = sd(CriticsScore), meanCs = mean(CriticsScore)) %>%
  ggplot(aes(x = ReleaseDate, y = CriticsScore)) +
  geom_line() +
  ylim(0, 100) +
  geom_hline(yintercept = 71, color = "grey") +
  geom_ribbon(aes(ymin = 71 - (1.7 * 3), ymax = 71 + (1.7 * 3)), alpha = 0.1, fill = "Blue")
