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
  filter(Platform %in% c("Nintendo 64", "GameCube", "Game Boy Advance", "3DS", "Wii", "Wii U", "Switch", "DS")) %>%
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

dreamcast_publishers <- clean_base %>%
  filter(Platform == "Dreamcast") %>%
  mutate(Publisher = str_split(Publisher, " , ")) %>%
  select(Publisher) %>%
  unlist() %>%
  unique()

exclusive_publishers <- tibble(Platform = c("PC", "iOS", "Sony", "Xbox", "Nintendo", "Dreamcast"), Share = c(
  sum(!(pc_publishers %in% c(sony_publishers, xbox_publishers, ios_publishers, nintendo_publishers, dreamcast_publishers))) / length(pc_publishers),
  sum(!(ios_publishers %in% c(sony_publishers, xbox_publishers, pc_publishers, nintendo_publishers, dreamcast_publishers))) / length(ios_publishers),
  sum(!(sony_publishers %in% c(ios_publishers, xbox_publishers, pc_publishers, nintendo_publishers, dreamcast_publishers))) / length(sony_publishers),
  sum(!(xbox_publishers %in% c(ios_publishers, sony_publishers, pc_publishers, nintendo_publishers, dreamcast_publishers))) / length(xbox_publishers),
  sum(!(nintendo_publishers %in% c(ios_publishers, xbox_publishers, pc_publishers, sony_publishers, dreamcast_publishers))) / length(nintendo_publishers),
  sum(!(dreamcast_publishers %in% c(ios_publishers, xbox_publishers, pc_publishers, sony_publishers, nintendo_publishers))) / length(dreamcast_publishers)))

png(filename="publishers.png", width = 900, height = 500)
ggplot(exclusive_publishers, aes(x = reorder(Platform, desc(Share)), y = Share), color = Platform) +
  geom_col() +
  geom_label(aes(label = as.integer(Share*100)), nudge_y = 0.05) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Платформа", y = NULL,
       title = "Эксклюзивные издатели",
       subtitle = "Доля издателей, выпускавших игры на одной платформе, %",
       caption = "Источник данных: metacritic.com, 01.07.2020") +
  theme(text = element_text(size = 16), legend.position="none")
dev.off()