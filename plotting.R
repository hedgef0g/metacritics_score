library(tidyverse)
meta_base <- read_tsv("metacritics_score.csv")
meta_base %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate != 1995) %>%
  group_by(ReleaseDate) %>%
  summarise(correlation = cor(UsersScore, CriticsScore)) %>%
  ungroup() %>%
  ggplot(aes(x = ReleaseDate, y = correlation, fill = correlation)) +
  geom_col() +
  ylim(0, 1) +
  xlim(1996, 2020) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 1)) +
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
