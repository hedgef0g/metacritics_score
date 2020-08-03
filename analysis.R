install.packages("gt")
library(gt)

games_by_platform <- clean_base %>%
  group_by(Platform) %>%
  summarize(Count = n())

png(filename="games.png", width = 900, height = 500)
ggplot(games_by_platform, aes(y = reorder(Platform, Count), x = Count)) +
  geom_segment(aes(xend = 0, yend = Platform, size = 10), color = "DarkBlue") +
  geom_label(aes(label = Count), nudge_x = 1500) +
  labs(x = "Количество игр (записей)", y = "Платформа",
       title ="Число игр (записей) в базе metacritic.com по платформам",
       caption = "Источник данных: metacritic.com, 01.07.2020",
       size = 32) +
  theme(text = element_text(size = 16), legend.position="none") 
dev.off()

pc_games <- clean_base %>%
  filter(Platform == "PC") %>%
  select(Title)

clean_base %>%
  group_by(Platform) %>%
  mutate(is_on_pc = Title %in% c(pc_games$Title)) %>%
  summarise(Count = n(), MScores = sum(!is.na(MetaScore)), UScores = sum(!is.na(UserScore)),
            Both = sum(!is.na(MetaScore) & !is.na(UserScore)), Share_of_both = Both / Count * 100) %>%
  mutate(Group = 
           ifelse(Platform %in% c("PC", "iOS"), "Other",
                  ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                         ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo")))) %>%
  arrange(desc(Share_of_both)) %>%
  rename("Кол-во игр" = Count, "С оценкой критиков" = MScores, "С оценкой пользователей" = UScores,
               "С 2-мя оценками" = Both, "Доля игр с 2-мя оценками, %" = Share_of_both) %>%
  gt(rowname_col = "Platform", groupname_col = "Group") %>%
  fmt_number(columns = vars("Доля игр с 2-мя оценками, %"), decimals = 0) %>%
  tab_header(
    title = md("Число оценённых игр на каждой из платформ")
  ) %>%
  tab_source_note(md("Источник данных: metacritic.com, 01.07.2020"))

rm(pc_games)

## Check how many publishers only publish games on specific platform
ios_publishers <- clean_base %>%
  filter(Platform == "iOS") %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()

sony_publishers <- clean_base %>%
  filter(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita")) %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()

xbox_publishers <- clean_base %>%
  filter(Platform %in% c("Xbox", "Xbox 360", "Xbox One")) %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()
  
nintendo_publishers <- clean_base %>%
  filter(Platform %in% c("Dreamcast", "Nintendo 64", "GameCube", "Game Boy Advance", "3DS", "Wii", "Wii U", "Switch", "DS")) %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()

pc_publishers <- clean_base %>%
  filter(Platform == "PC") %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()

exclusive_publishers <- tibble(Platform = c("PC", "iOS", "Sony", "Xbox", "Nintendo"), Share = c(
  sum(!(pc_publishers %in% c(sony_publishers, xbox_publishers, ios_publishers, nintendo_publishers))) / length(pc_publishers),
  sum(!(ios_publishers %in% c(sony_publishers, xbox_publishers, pc_publishers, nintendo_publishers))) / length(ios_publishers),
  sum(!(sony_publishers %in% c(ios_publishers, xbox_publishers, pc_publishers, nintendo_publishers))) / length(sony_publishers),
  sum(!(xbox_publishers %in% c(ios_publishers, sony_publishers, pc_publishers, nintendo_publishers))) / length(xbox_publishers),
  sum(!(nintendo_publishers %in% c(ios_publishers, xbox_publishers, pc_publishers, sony_publishers))) / length(nintendo_publishers)))

png(filename="publishers.png", width = 900, height = 500)
ggplot(exclusive_publishers, aes(x = reorder(Platform, desc(Share)), y = Share)) +
  geom_segment(aes(xend = Platform, yend = 0, size = 10), color = "DarkBlue") +
  geom_label(aes(label = as.integer(Share*100)), nudge_y = 0.05) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Платформа", y = NULL,
       title = "Эксклюзивные издатели",
       subtitle = "Доля издателей, выпускавших игры на одной платформе, %",
       caption = "Источник данных: metacritic.com, 01.07.2020") +
  theme(text = element_text(size = 16), legend.position="none")
dev.off()

## Check how many exclusive titles are there on each platform
exclusive_games <- tibble(Platform = unique(clean_base$Platform), Share = c(
  sum(!(filter(clean_base, Platform == "PC")$Title %in% filter(clean_base, Platform != "PC")$Title)) / nrow(filter(clean_base, Platform == "PC")),
  sum(!(filter(clean_base, Platform == "PlayStation")$Title %in% filter(clean_base, Platform != "PlayStation")$Title)) / nrow(filter(clean_base, Platform == "PlayStation")),
  sum(!(filter(clean_base, Platform == "PlayStation 2")$Title %in% filter(clean_base, Platform != "PlayStation 2")$Title)) / nrow(filter(clean_base, Platform == "PlayStation 2")),
  sum(!(filter(clean_base, Platform == "PlayStation 3")$Title %in% filter(clean_base, Platform != "PlayStation 3")$Title)) / nrow(filter(clean_base, Platform == "PlayStation 3")),
  sum(!(filter(clean_base, Platform == "PlayStation 4")$Title %in% filter(clean_base, Platform != "PlayStation 4")$Title)) / nrow(filter(clean_base, Platform == "PlayStation 4")),
  sum(!(filter(clean_base, Platform == "PSP")$Title %in% filter(clean_base, Platform != "PSP")$Title)) / nrow(filter(clean_base, Platform == "PSP")),
  sum(!(filter(clean_base, Platform == "PlayStation Vita")$Title %in% filter(clean_base, Platform != "PlayStation Vita")$Title)) / nrow(filter(clean_base, Platform == "PlayStation Vita")),
  sum(!(filter(clean_base, Platform == "Xbox")$Title %in% filter(clean_base, Platform != "Xbox")$Title)) / nrow(filter(clean_base, Platform == "Xbox")),
  sum(!(filter(clean_base, Platform == "Xbox 360")$Title %in% filter(clean_base, Platform != "Xbox 360")$Title)) / nrow(filter(clean_base, Platform == "Xbox 360")),
  sum(!(filter(clean_base, Platform == "Xbox One")$Title %in% filter(clean_base, Platform != "Xbox One")$Title)) / nrow(filter(clean_base, Platform == "Xbox One")),
  sum(!(filter(clean_base, Platform == "3DS")$Title %in% filter(clean_base, Platform != "3DS")$Title)) / nrow(filter(clean_base, Platform == "3DS")),
  sum(!(filter(clean_base, Platform == "DS")$Title %in% filter(clean_base, Platform != "DS")$Title)) / nrow(filter(clean_base, Platform == "DS")),
  sum(!(filter(clean_base, Platform == "GameCube")$Title %in% filter(clean_base, Platform != "GameCube")$Title)) / nrow(filter(clean_base, Platform == "GameCube")),
  sum(!(filter(clean_base, Platform == "Game Boy Advance")$Title %in% filter(clean_base, Platform != "Game Boy Advance")$Title)) / nrow(filter(clean_base, Platform == "Game Boy Advance")),
  sum(!(filter(clean_base, Platform == "Nintendo 64")$Title %in% filter(clean_base, Platform != "Nintendo 64")$Title)) / nrow(filter(clean_base, Platform == "Nintendo 64")),
  sum(!(filter(clean_base, Platform == "Switch")$Title %in% filter(clean_base, Platform != "Switch")$Title)) / nrow(filter(clean_base, Platform == "Switch")),
  sum(!(filter(clean_base, Platform == "Wii")$Title %in% filter(clean_base, Platform != "Wii")$Title)) / nrow(filter(clean_base, Platform == "Wii")),
  sum(!(filter(clean_base, Platform == "Wii U")$Title %in% filter(clean_base, Platform != "Wii U")$Title)) / nrow(filter(clean_base, Platform == "Wii U")),
  sum(!(filter(clean_base, Platform == "Dreamcast")$Title %in% filter(clean_base, Platform != "Dreamcast")$Title)) / nrow(filter(clean_base, Platform == "Dreamcast")),
  sum(!(filter(clean_base, Platform == "iOS")$Title %in% filter(clean_base, Platform != "iOS")$Title)) / nrow(filter(clean_base, Platform == "iOS"))),
  Group = ifelse(Platform %in% c("PC", "iOS"), "Other",
                 ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                        ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo"))))

png(filename = "exclusives.png", width = 900, height = 500)
ggplot(exclusive_games) +
  geom_col(aes(x = Share, y = reorder(Platform, Share), fill = Group)) +
  geom_label(aes(label = as.integer(Share*100), x = Share + 0.03, y = Platform)) +
  scale_x_continuous(labels = scales::percent) + 
  labs(y = "Платформа", x = NULL,
       title = "Эксклюзивные игры",
       subtitle = "Доля игр, не выходивших за пределами платформы, %",
       caption = "Источник данных: metacritic.com, 01.07.2020",
       fill = "Владелец\nплатформы") +
  theme(text = element_text(size = 16))
dev.off()

## Compare similarity of top genres between platforms

PC_genres <- clean_base %>%
  filter(Platform == "PC") %>%
  select(Platform, Genres) %>%
  summarize(Genres = unlist(str_split(Genres, ", "))) %>%
  group_by(Genres) %>%
  summarise(num_games = n()) %>%
  arrange(desc(num_games))

iOS_genres <- clean_base %>%
  filter(Platform == "iOS") %>%
  select(Platform, Genres) %>%
  summarize(Genres = unlist(str_split(Genres, ", "))) %>%
  group_by(Genres) %>%
  summarise(num_games = n()) %>%
  arrange(desc(num_games))

clean_base %>%
  filter(is.na(ReleaseDate))

scores_summary <- clean_base %>%
  select(MetaScore, UserScore) %>%
  summary()

scores_summary

sum(!is.na(clean_base$MetaScore))
sum(!is.na(clean_base$MetaScore)) / nrow(clean_base)
sum(!is.na(clean_base$UserScore))
sum(!is.na(clean_base$UserScore)) / nrow(clean_base)
sum(!is.na(clean_base$MetaScore) & !is.na(clean_base$UserScore))
sum(!is.na(clean_base$MetaScore) & !is.na(clean_base$UserScore))/nrow(clean_base)

clean_base %>%
  filter(Platform == "iOS") %>%
  summarize(share_of_user_reviews = sum(!is.na(UserScore))/ n(), share_of_ctritics_review = sum(!is.na(MetaScore))/ n())

MetaScore <- data.frame(score = clean_base$MetaScore, year = year(clean_base$ReleaseDate))
UserScore <- data.frame(score = clean_base$UserScore * 10, year = year(clean_base$ReleaseDate))

MetaScore$source <- "Критики"
UserScore$source <- "Пользователи"

scores <- rbind(MetaScore, UserScore)

scores %>%
  filter(year >= 2001 & year <= 2020) %>%
  ggplot(aes(x = score, fill = source)) +
    geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', binwidth = 5) +
    facet_wrap(~ year) +
    labs(x = "Оценка по 100-балльной шкале", y = "Доля оценок",
         title ="Распределение пользовательских оценок\n и оценок профильной прессы",
         caption = "Источник данных: metacritic.com, 01.07.2020") +
    guides(fill = guide_legend(title = "Источник оценки:"))

rm(list = c(MetaScore, UserScore))

max(lengths(str_split(clean_base$Developer, " , ")))
sort(unique(unlist(str_split(clean_base$Genres, ", "))))
length(unique(unlist(str_split(clean_base$Genres, ", "))))
max(lengths(str_split(clean_base$Genres, ", ")))

clean_base %>%
  group_by(year = year(ReleaseDate)) %>%
  summarize(MeanUserScore = mean(UserScore), MeanMetaScore = mean(MetaScore))

clean_base %>%
  filter(!is.na(MetaScore)) %>%
  ggplot(aes(x = CriticReviews)) +
  geom_histogram()
