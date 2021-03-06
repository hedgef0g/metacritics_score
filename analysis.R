install.packages("gt")
install.packages("textcat")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("nortest")

library(gt)
library(textcat)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(ggpubr)

clean_base %>%
  group_by(Platform) %>%
  summarise(Count = n(), MScores = sum(!is.na(MetaScore)), UScores = sum(!is.na(UserScore)),
            Both = sum(!is.na(MetaScore) & !is.na(UserScore)), Share_of_both = Both / Count * 100) %>%
  mutate(Group = 
           ifelse(Platform %in% c("PC", "iOS", "Dreamcast"), "Other",
                  ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                         ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo")))) %>%
  arrange(desc(Share_of_both)) %>%
  rename("���-�� ���" = Count, "� ������� ��������" = MScores, "� ������� �������������" = UScores,
         "� 2-�� ��������" = Both, "���� ��� � 2-�� ��������, %" = Share_of_both) %>%
  gt(rowname_col = "Platform", groupname_col = "Group") %>%
  fmt_number(columns = vars("���� ��� � 2-�� ��������, %"), decimals = 0) %>%
  tab_header(
    title = md("����� ��������� ��� �� ������ �� ��������")
  ) %>%
  tab_source_note(md("�������� ������: metacritic.com, 01.07.2020"))

both_scores <- clean_base %>%
  filter(!is.na(MetaScore) & !is.na(UserScore))

## Amount of games by platform & share of games with both critics and users scores
games_by_platform <- clean_base %>%
  group_by(Platform) %>%
  summarize(Count = n()) %>%
  mutate(Owner = ifelse(Platform %in% c("PC", "iOS", "Dreamcast"), "Other",
                        ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                               ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo"))))

games_by_platform_scored <- both_scores %>%
  group_by(Platform) %>%
  summarize(Count = n()) %>%
  mutate(Owner = ifelse(Platform %in% c("PC", "iOS", "Dreamcast"), "Other",
                        ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                               ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo"))),
         Share = Count/games_by_platform$Count*100)

png(filename="games.png", width = 900, height = 500)
ggplot(games_by_platform, aes(y = reorder(Platform, Count), x = Count), fill = "DarkGrey") +
  geom_col() +
  geom_label(aes(label = Count), nudge_x = 2000, fill = "white") +
  geom_col(data = games_by_platform_scored, aes(y = Platform, x = Count, fill = Owner)) +
  geom_label(data = games_by_platform_scored, aes(label = round(Share, 0)), x = -1500, fill = "LightGrey", size = 3) +
  xlim(-1500, 54000) +
  labs(x = "���������� ��� (�������)", y = "���������",
       title ="����� ��� (�������) � ���� metacritic.com �� ����������",
       caption = "�������� ������: metacritic.com, 01.07.2020",
       size = 32,
       fill = "��������\n���������") +
  theme(text = element_text(size = 16)) 
dev.off()

games_by_year <- clean_base %>%
  mutate(year = year(ReleaseDate), month = month(ReleaseDate)) %>%
  filter(year <= 2020) %>%
  group_by(year, month, Platform) %>%
  summarise(Count = n())

games_by_month <- clean_base %>%
  mutate(date = format(ReleaseDate, "%Y-%m")) %>%
  filter(!is.na(ReleaseDate) & year(ReleaseDate) %in% seq(2000, 2020)) %>%
  group_by(date) %>%
  summarise(n = sum(n())) %>%
  mutate(month = str_sub(date, start = -2))

year_start <- games_by_month$date[str_detect(games_by_month$date, "-12")]

by_year <- games_by_year %>%
  filter(year %in% seq(2000, 2020)) %>%
  ggplot(aes(x = year, y = Count)) +
  geom_col(fill = "LightBlue4") + 
  geom_vline(xintercept = seq(2000.5, 2020.5), color = "DarkGrey") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.8), "cm")) +
  scale_x_discrete(breaks = NULL, expand = c(0.01, 0.01)) +
  labs(y = "����� ���",
       title ="����� ��� (�������) � ���� metacritic.com �� ���� ������ ����",
       size = 32) +
  theme(text = element_text(size = 16)) 

by_half_year <- games_by_year %>%
  filter(year %in% seq(2000, 2020)) %>%
  mutate(date = ifelse(month %in% seq(1, 6), str_c(year, 1, sep = "-"), str_c(year, 2, sep = "-"))) %>%
  group_by(date) %>%
  ggplot(aes(x = date, y = Count)) +
  geom_col(fill = "LightBlue4") +
  geom_vline(xintercept = seq(2.5, 41.5, by = 2), color = "DarkGrey") +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,1), "cm"),
        text = element_text(size = 16)) +
  labs(y = "����� ���")

by_month <- ggplot(data = games_by_month, aes(x = date, y = n)) +
  geom_col(fill = "LightBlue4") +
  geom_vline(xintercept = year_start, color = "DarkGrey") +
  theme(legend.position = "none",
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  plot.margin = unit(c(0.5,0.5,0.5,1), "cm"),
  text = element_text(size = 16)) +
  scale_x_discrete(breaks = NULL) +
  annotate("text", x = games_by_month$date[str_detect(games_by_month$date, "-06")], y = -50, label = seq(2000, 2020)) +
  labs(x = "���", y = "����� ���",
       caption = "�������� ������: metacritic.com, 01.07.2020")

png(filename="games_by_time.png", width = 900, height = 500)
gridExtra::grid.arrange(by_year, by_half_year, by_month, nrow = 3)
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
  Group = ifelse(Platform %in% c("PC", "iOS", "Dreamcast"), "Other",
                 ifelse(Platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PSP", "PlayStation Vita"), "Sony",
                        ifelse(Platform %in% c("Xbox", "Xbox 360", "Xbox One"), "Microsoft", "Nintendo"))),
  Evals = c(
    nrow(filter(clean_base, Platform == "PC" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PC")),
    nrow(filter(clean_base, Platform == "PlayStation" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PlayStation")),
    nrow(filter(clean_base, Platform == "PlayStation 2" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PlayStation 2")),
    nrow(filter(clean_base, Platform == "PlayStation 3" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PlayStation 3")),
    nrow(filter(clean_base, Platform == "PlayStation 4" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PlayStation 4")),
    nrow(filter(clean_base, Platform == "PSP" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PSP")),
    nrow(filter(clean_base, Platform == "PlayStation Vita" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "PlayStation Vita")),
    nrow(filter(clean_base, Platform == "Xbox" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Xbox")),
    nrow(filter(clean_base, Platform == "Xbox 360" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Xbox 360")),
    nrow(filter(clean_base, Platform == "Xbox One" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Xbox One")),
    nrow(filter(clean_base, Platform == "3DS" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "3DS")),
    nrow(filter(clean_base, Platform == "DS" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "DS")),
    nrow(filter(clean_base, Platform == "GameCube" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "GameCube")),
    nrow(filter(clean_base, Platform == "Game Boy Advance" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Game Boy Advance")),
    nrow(filter(clean_base, Platform == "Nintendo 64" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Nintendo 64")),
    nrow(filter(clean_base, Platform == "Switch" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Switch")),
    nrow(filter(clean_base, Platform == "Wii" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Wii")),
    nrow(filter(clean_base, Platform == "Wii U" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Wii U")),
    nrow(filter(clean_base, Platform == "Dreamcast" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "Dreamcast")),
    nrow(filter(clean_base, Platform == "iOS" & !is.na(MetaScore) & !is.na(UserScore))) / nrow(filter(clean_base, Platform == "iOS"))
  ))

png(filename = "exclusives.png", width = 900, height = 500)
ggplot(exclusive_games) +
  geom_col(aes(x = Share, y = reorder(Platform, Share)), fill = "DarkGrey") +
  geom_col(aes(x = Share*Evals, y = reorder(Platform, Share), fill = Group)) +
  geom_label(aes(label = as.integer(Share*100), x = Share + 0.03, y = Platform)) +
  geom_label(aes(label = as.integer(Evals*100), x = -0.03, y = Platform), size = 3, fill = "LightGray") +
  scale_x_continuous(labels = scales::percent) + 
  labs(y = "���������", x = NULL,
       title = "������������ ����",
       subtitle = "���� ���, �� ���������� �� ��������� ���������, %",
       caption = "�������� ������: metacritic.com, 01.07.2020",
       fill = "��������\n���������") +
  theme(text = element_text(size = 16))
dev.off()

## Summary on scores
scores_summary <- both_scores %>%
  select(MetaScore, UserScore) %>%
  summary()

scores_summary

vlines <- both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  summarize(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews))

## Meta and User scores - Total
png(filename = "scores.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  ggplot() +
  geom_histogram(alpha = 0.4, aes(x = MetaScore, y = ..density.., fill = "r"), binwidth = 5) +
  geom_histogram(alpha = 0.4, aes(x = UserScore * 10, y = ..density.., fill = "b"), binwidth = 5) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(data = vlines, aes(xintercept = mMS), color = "#F8766D") +
  geom_vline(data = vlines, aes(xintercept = mUS * 10), color = "#00BFC4") +
  geom_label(data = vlines, aes(label = as.integer(mMS), x = mMS + 5, y = 0.04), color = "#F8766D", size = 5) +
  geom_label(data = vlines, aes(label = as.integer(mUS * 10), x = (mUS * 10) - 5, y = 0.04), color = "#00BFC4", size = 5) +
  labs(x = "������ �� 100-�������� �����", y = "���� ������",
       title = "������������� ���������������� ������ � ������ ���������� ������",
       subtitle = "��� ����������� �� ���� ������ ����",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

## Plotting scores with low number of reviews
png(filename = "scores_small_scale.png", width = 900, height = 500)
ggplot(both_scores) +
  geom_histogram(alpha = 0.4, aes(x = MetaScore, y = ..density.., fill = "r"), binwidth = 1) +
  geom_histogram(alpha = 0.4, aes(x = UserScore * 10, y = ..density.., fill = "b"), binwidth = 1) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, 100, by = 5), labels = seq(0, 100, by = 5)) +
  geom_vline(data = vlines, aes(xintercept = mMS), color = "#F8766D") +
  geom_vline(data = vlines, aes(xintercept = mUS * 10), color = "#00BFC4") +
  geom_label(data = vlines, aes(label = as.integer(mMS), x = mMS + 5, y = 0.04), color = "#F8766D", size = 5) +
  geom_label(data = vlines, aes(label = as.integer(mUS * 10), x = (mUS * 10) - 5, y = 0.04), color = "#00BFC4", size = 5) +
  labs(x = "������ �� 100-�������� �����", y = "���� ������",
       title = "������������� ���������������� ������ � ������ ���������� ������",
       subtitle = "��� ����������� �� ���� ������ ����",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank()) 
dev.off()

## Compare scores - paired t-test

t.test(x = both_scores$UserScore * 10,
       y = both_scores$MetaScore,  
       paired = TRUE)

ggplot(data = both_scores) +
  geom_boxplot(aes(x = 1, y = MetaScore)) +
  geom_boxplot(aes(x = 2, y = UserScore * 10))

summary(both_scores)

## Meta and User scores by year
vlines <- both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2001) %>%
  group_by(ReleaseDate) %>%
  summarize(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews))

png(filename = "scores_by_year.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2001) %>%
  ggplot() +
  geom_histogram(alpha = 0.4, aes(x = MetaScore, y = ..density.., fill = "r"), binwidth = 5) +
  geom_histogram(alpha = 0.4, aes(x = UserScore * 10, y = ..density.., fill = "b"), binwidth = 5) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(data = vlines, aes(xintercept = mMS), color = "#F8766D") +
  geom_vline(data = vlines, aes(xintercept = mUS * 10), color = "#00BFC4") +
  geom_label(data = vlines, aes(label = as.integer(mMS), x = 10, y = 0.03), color = "#F8766D") +
  geom_label(data = vlines, aes(label = as.integer(mUS * 10), x = 10, y = 0.045), color = "#00BFC4") +
  facet_wrap(~ ReleaseDate) +
  labs(x = "������ �� 100-�������� �����", y = "���� ������",
       title = "������������� ���������������� ������ � ������ ���������� ������",
       subtitle = "� ����������� �� ���� ������ ����",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

## Meta and User scores by platform
vlines <- both_scores %>%
  group_by(Platform) %>%
  summarize(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews))

png(filename = "scores_by_platform.png", width = 900, height = 500)
both_scores %>%
  ggplot() +
  geom_histogram(alpha = 0.4, aes(x = MetaScore, y = ..density.., fill = "r"), binwidth = 5) +
  geom_histogram(alpha = 0.4, aes(x = UserScore * 10, y = ..density.., fill = "b"), binwidth = 5) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(data = vlines, aes(xintercept = mMS), color = "#F8766D") +
  geom_vline(data = vlines, aes(xintercept = mUS * 10), color = "#00BFC4") +
  geom_label(data = vlines, aes(label = as.integer(mMS), x = 10, y = 0.03), color = "#F8766D") +
  geom_label(data = vlines, aes(label = as.integer(mUS * 10), x = 10, y = 0.045), color = "#00BFC4") +
  facet_wrap(~ Platform) +
  labs(x = "������ �� 100-�������� �����", y = "���� ������",
       title = "������������� ���������������� ������ � ������ ���������� ������",
       subtitle = "� ����������� �� ���� ������ ����",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

rm(vlines)

## How scores changed since 2000
png(filename = "change_in_means.png", width = 900, height = 500)
both_scores %>%
  mutate(year = year(ReleaseDate)) %>%
  filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews)) %>%
  ggplot(aes(x = year)) + 
  geom_col(aes(y = mMS, fill = "r"), alpha = 0.4) +
  geom_col(aes(y = mUS * 10, fill = "b"), alpha = 0.4) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  geom_hline(aes(yintercept = weighted.mean(both_scores$MetaScore, both_scores$CriticReviews)), color = "#F8766D", size = 2) +
  geom_hline(aes(yintercept = weighted.mean(both_scores$UserScore, both_scores$UserReviews) * 10), color = "#00BFC4", size = 2) +
  geom_label(aes(label = as.integer(mMS), x = year, y = 95), fill = "#F8766D", alpha = 0.2) +
  geom_label(aes(label = as.integer(mUS * 10), x = year, y = 100), fill = "#00BFC4", alpha = 0.2) +
  labs(x = "���", y = "������� ������",
       title = "��������� ������� ������ ������������� � �������� � ����������� �� ���� ������ ����",
       subtitle = "� ������ � 2000 �� 2020 ��",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

## How the correlation changes since 2000
png(filename = "change_in_correlation.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2000) %>%
  group_by(ReleaseDate) %>%
  summarise(correlation = cor(UserScore, MetaScore)) %>%
  ggplot(aes(x = ReleaseDate, y = correlation, fill = correlation)) +
  geom_col() +
  geom_hline(aes(yintercept = cor(both_scores$UserScore, both_scores$MetaScore)), size = 1, color = "Grey") +
  geom_label(aes(x = ReleaseDate, y = correlation + 0.05, label = round(correlation, 3)), fill = "White") +
  ylim(0, 1) +
  labs(x = "��� ������ ����", y = "���������� ����� �������� �������� � �������",
       title ="��������� ��������� ������� ���������� � ������?",
       subtitle = "���������� ����� �������� �������� � ������� (����� � 1 = �������)",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16))
dev.off()

## Scores and correlation without outliers (by # of reviews)
png(filename = "number_of_scores.png", width = 900, height = 500)
both_scores %>%
  arrange(desc(UserReviews)) %>%
  ggplot() +
  geom_point(aes(x = CriticReviews, y = UserReviews, alpha = 0.3), position = "jitter") +
  geom_label(aes(label = Title[1], x = CriticReviews[1], y = 100000)) +
  geom_label(aes(label = Title[2], x = CriticReviews[2], y = 25000)) +
  labs(x = "����� ������ ��������", y = "����� ������ �������",
       title = "����� ������ �������� vs ����� ������ �������",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16))
dev.off()

both_scores %>%
  arrange(desc(UserReviews)) %>%
  select(Title, Platform, ReleaseDate, UserScore, UserReviews, MetaScore, CriticReviews) %>%
  head(10) %>%
  rename("��������" = Title, "���������" = Platform, "���� ������" = ReleaseDate,
         "������ �������" = UserScore, "����� ������ �������" = UserReviews,
         "������ ��������" = MetaScore, "����� �������" = CriticReviews) %>%
  gt() %>%
  tab_header(
    title = md("���-10 ��� �� ����� ���������������� ������")
  ) %>%
  tab_source_note(md("�������� ������: metacritic.com, 01.07.2020"))

png(filename = "mean_scores_cleaned_1.png", width = 900, height = 500)
both_scores %>%
  mutate(year = year(ReleaseDate)) %>%
  filter(year >= 2000 & UserReviews < 30000) %>%
  group_by(year) %>%
  summarise(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews)) %>%
  ggplot(aes(x = year)) + 
  geom_col(aes(y = mMS, fill = "r"), alpha = 0.4) +
  geom_col(aes(y = mUS * 10, fill = "b"), alpha = 0.4) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  geom_hline(aes(yintercept = weighted.mean(both_scores$MetaScore, both_scores$CriticReviews)), color = "#F8766D", size = 2) +
  geom_hline(aes(yintercept = weighted.mean(both_scores$UserScore * 10, both_scores$UserReviews)), color = "#00BFC4", size = 2) +
  geom_label(aes(label = as.integer(mMS), x = year, y = 95), fill = "#F8766D", alpha = 0.2) +
  geom_label(aes(label = as.integer(mUS * 10), x = year, y = 100), fill = "#00BFC4", alpha = 0.2) +
  labs(x = "���", y = "������� ������",
       title = "��������� ������� ������ ������������� � �������� � ����������� �� ���� ������ ����",
       subtitle = "� ������ � 2000 �� 2020 ��, �� ����������� W3:Reforged � TLoU.P2",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

png(filename = "correlations_cleaned_1.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2000 & UserReviews < 30000) %>%
  group_by(ReleaseDate) %>%
  summarise(correlation = cor(UserScore, MetaScore)) %>%
  mutate(meancor = mean(correlation), sdcor = sd(correlation)) %>%
  ggplot(aes(x = ReleaseDate, y = correlation, fill = correlation)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(correlation)), size = 1, color = "Grey") +
  geom_label(aes(x = ReleaseDate, y = correlation + 0.05, label = round(correlation, 3)), fill = "White") +
  ylim(0, 1) +
  labs(x = "��� ������ ����", y = "���������� ����� �������� �������� � �������",
       title ="��������� ��������� ������� ���������� � ������?",
       subtitle = "���������� ����� �������� �������� � ������� (����� � 1 = �������)",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16))
dev.off()

without_US_outliers <- both_scores %>%
  filter(UserReviews < as.numeric(summary(both_scores$UserReviews)[5]) + 96)

nrow(without_US_outliers)
nrow(both_scores) - nrow(without_US_outliers)

png(filename = "mean_scores_cleaned_2.png", width = 900, height = 500)
without_US_outliers %>%
  mutate(year = year(ReleaseDate)) %>%
  filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(mMS = weighted.mean(MetaScore, CriticReviews), mUS = weighted.mean(UserScore, UserReviews)) %>%
  ggplot(aes(x = year)) + 
  geom_col(aes(y = mMS, fill = "r"), alpha = 0.4) +
  geom_col(aes(y = mUS * 10, fill = "b"), alpha = 0.4) +
  scale_fill_manual(name ="scores", values = c("r" = "#F8766D", "b" = "#00BFC4"), labels = c("b" = "������������", "r" = "�������")) +
  geom_hline(aes(yintercept = mean(filter(without_US_outliers, year(ReleaseDate) >= 2000)$MetaScore)), color = "#F8766D", size = 2) +
  geom_hline(aes(yintercept = mean(filter(without_US_outliers, year(ReleaseDate) >= 2000)$UserScore) * 10), color = "#00BFC4", size = 2) +
  geom_label(aes(label = as.integer(mMS), x = year, y = 95), fill = "#F8766D", alpha = 0.2) +
  geom_label(aes(label = as.integer(mUS * 10), x = year, y = 100), fill = "#00BFC4", alpha = 0.2) +
  labs(x = "���", y = "������� ������",
       title = "��������� ������� ������ ������������� � �������� � ����������� �� ���� ������ ����",
       subtitle = "� ������ � 2000 �� 2020 ��, ��� ����� ��� � ������� ������ ���������������� �������",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  guides(fill = guide_legend(title = "�������� ������:")) +
  theme(text = element_text(size = 16))
dev.off()

png(filename = "correlations_cleaned_2.png", width = 900, height = 500)
without_US_outliers %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(ReleaseDate >= 2000) %>%
  group_by(ReleaseDate) %>%
  summarise(correlation = cor(UserScore, MetaScore)) %>%
  mutate(meancor = mean(correlation), sdcor = sd(correlation)) %>%
  ggplot(aes(x = ReleaseDate, y = correlation, fill = correlation)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(correlation)), size = 1, color = "Grey") +
  geom_label(aes(x = ReleaseDate, y = correlation + 0.05, label = round(correlation, 3)), fill = "White") +
  ylim(0, 1) +
  labs(x = "��� ������ ����", y = "���������� ����� �������� �������� � �������",
       title ="��������� ��������� ������� ���������� � ������?",
       subtitle = "���������� ����� �������� �������� � ������� (����� � 1 = �������),\n��� ����� ��� � ������� ������ ���������������� �������",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16))
dev.off()


## Visualizing difference in scores
diff_plot <- both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate), Diff = UserScore * 10 - MetaScore) %>%
  filter(ReleaseDate >= 2001) %>%
  ggplot() +
  geom_histogram(aes(x = Diff, y = ..density.., fill = ..x..), binwidth = 1) +
  scale_y_continuous(labels = scales::percent)

png(filename = "difference.png", width = 900, height = 500)
diff_plot +
  # geom_vline(aes(xintercept = mean(Diff))) +
  # geom_vline(aes(xintercept = median(Diff))) +
  xlim(-75, 70) +
  geom_label(aes(label = "<- ���� ������ ��������", x = -60, y = 0.045), color = "DarkBlue", size = 6) +
  geom_label(aes(label = "���� ������ ������� ->", x = 58, y = 0.045), color = "Blue", size = 6) +
  labs(x = "������� � ������", y = "���� ��� � ������ ��������",
       title = "������������� ������� � ������� ������� � ��������",
       subtitle = "(������ ������� * 10 - ������ ��������)",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16))
dev.off()

Diff <- unlist(both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate), Diff = UserScore * 10 - MetaScore) %>%
  select(Diff))

nortest::pearson.test(Diff)

png(filename = "diffrence_by_year.png", width = 900, height = 500)
diff_plot +
  facet_wrap(~ ReleaseDate) +
  labs(x = "������� � ������", y = "���� ��� � ������ ��������",
     title = "������������� ������� � ������� ������� � ��������",
     subtitle = "� ����������� �� ���� ������ ����",
     caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16)) 
dev.off()

png(filename = "mean_diffrence_by_year.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate), Diff = (UserScore * 10 - MetaScore)) %>%
  filter(ReleaseDate >= 2001) %>%
  group_by(ReleaseDate) %>%
  summarize(Diff = mean(Diff)) %>%
  ggplot() +
  geom_col(aes(x = ReleaseDate, y = Diff, fill = Diff)) +
  labs(x = "��� ������ ����", y = "������� ������� � �������",
       title = "������������� ������� � ������� ������� � ��������",
       subtitle = "����������� ������� �� ���",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16)) 
dev.off()

png(filename = "abs_difference_by_platform.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate), Diff = (UserScore * 10 - MetaScore)) %>%
  group_by(Platform) %>%
  summarize(Diff = mean(Diff)) %>%
  arrange(desc(Diff)) %>%
  ggplot() +
  geom_col(aes(x = reorder(Platform, -Diff), y = Diff, fill = Diff)) +
  labs(x = "", y = "������� ������� � �������",
       title = "������������� ������� � ������� ������� � ��������",
       subtitle = "����������� ������� ������ �� ���������",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) 
dev.off()

png(filename = "difference_by_platform.png", width = 900, height = 500)
diff_plot +
  facet_wrap(~ Platform) +
  labs(x = "������� � ������", y = "���� ��� � ������ ��������",
       title = "������������� ������� � ������� ������� � ��������",
       subtitle = "� ����������� �� ���������",
       caption = "�������� ������: metacritic.com, 01.07.2020") +
  theme(legend.position = "none",
        text = element_text(size = 16)) 
dev.off()

both_scores_cor <- both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(Platform %in% c("PC", "Xbox One", "Switch", "PlayStation 4", "iOS"), ReleaseDate >= 2013) %>%
  group_by(Platform) %>%
  summarise(correlation = str_c("���������� = ", round(cor(UserScore, MetaScore), 3)))

current_gen <- c("PC", "Xbox One", "Switch", "PlayStation 4", "iOS")

png(filename = "correlation_current_gen.png", width = 900, height = 500)
both_scores %>%
  mutate(ReleaseDate = year(ReleaseDate)) %>%
  filter(Platform %in% current_gen, ReleaseDate >= 2013) %>%
  ggplot(aes(x = UserScore, y = MetaScore, alpha = 0.1, color = Platform)) +
  geom_point() + 
  facet_wrap(Platform ~ .) +
  geom_text(x = 7.5, y = 20, aes(label = correlation), data = both_scores_cor, color = "black") +
  geom_smooth(color = "Light Blue", se = FALSE) +
  theme(legend.position = "none",
        text = element_text(size = 16)) +
  labs(x = "���������������� ������", y = "������ ��������",
       title ="����� ����� �������� �������� � �������",
       subtitle = "� ������� current gen ��������; ���� �������� � 2013 ����",
       caption = "�������� ������: metacritic.com, 01.07.2020")
dev.off()

top_publishers <- both_scores %>%
  group_by(Publisher) %>% 
  summarize(Games = n()) %>%
  arrange(desc(Games)) %>%
  filter(Games >= 75)

top_publishers <- as.vector(top_publishers$Publisher)

both_scores %>%
  filter(year(ReleaseDate) >= 2001 & Publisher %in% top_publishers[1:20]) %>%
  mutate(Diff = UserScore * 10 - MetaScore) %>%
  ggplot() +
  geom_histogram(aes(x = Diff, y = ..density..)) +
  facet_wrap(~ Publisher)
