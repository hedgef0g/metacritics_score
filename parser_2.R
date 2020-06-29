#install.packages("readr")
#install.packages("dplyr")
#install.packages("rvest")    
#install.packages("stringr")   
#install.packages("rebus")     
#install.packages("httr")

library(readr)
library(dplyr)
library(rvest)    
library(stringr)   
library(rebus)     
library(httr)

## Seeting up toolbox
# Getting last page for the exact first letter in the title
get_last_page <- function(html){
  html %>%
    html_nodes(".page.last_page .page_num") %>%
    html_text()
}
# Getting the link for the game
get_links <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".product_title a") %>%      
    html_attr("href") %>% 
    # Convert the list into a vector
    unlist()
}
# Getting the title 
get_title <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".hover_none h1") %>%      
    html_text() %>% 
    # Convert the list into a vector
    unlist()
}
# Getting the platform
get_platform <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".platform") %>%      
    html_text() %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[\r\n]", "") %>%
    # Convert the list into a vector
    unlist()
}
# Getting the developer
get_developer <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".summary_detail.developer span.data") %>%      
    html_text() %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[\r\n]", "") %>%
    # Convert the list into a vector
    unlist()
}
# Getting the publisher
get_publisher <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".summary_detail.publisher span.data") %>%      
    html_text() %>% 
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[\r\n]", "") %>%
    # Convert the list into a vector
    unlist()
}
# Getting release date
get_releasedate <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".summary_detail.release_data span.data") %>%      
    html_text() %>% 
    # Convert the list into a vector
    unlist()
}
# Getting userscore
get_userscore <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".metascore_anchor .metascore_w.user.large.game") %>%      
    html_text() %>% 
    # Convert the list into a vector
    unlist()
}
# Getting number of users reviews
get_userreviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes("div.userscore_wrap.feature_userscore div.summary p span.count a") %>%      
    html_text() %>%
    str_replace_all("[a-zA-Z]", "") %>%
    str_trim() %>%
    # Convert the list into a vector
    unlist()
}
# Getting metascore
get_metascore <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".xlarge") %>%      
    html_text() %>% 
    # Convert the list into a vector
    unlist()
}
# Getting number of critics reviews
get_criticreviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes("div.metascore_wrap.highlight_metascore div.summary p span.count a") %>%      
    html_text() %>% 
    str_replace_all("[a-zA-Z]", "") %>%
    str_replace_all("[\r\n]", "") %>%
    str_trim() %>%
    # Convert the list into a vector
    unlist()
}
# Getting genres
get_genres <- function(html){
  html %>% 
    # The relevant tag
    html_nodes(".product_genre span.data") %>%      
    html_text() %>%
    str_replace_all("[\r\n]", "") %>%
    str_trim() %>%
    # Convert the list into a vector
    unlist()
}
# Getting permanent link
links_base <- function(platform) {
  alphabetical_list <- paste("https://www.metacritic.com/browse/games/title/", platform, sep = "")
  pages_to_parse <- str_replace(sort(paste(alphabetical_list, rep(c("", paste("/", letters)), length(alphabetical_list)), sep = "")),"/ ", "/")
  full_games_list <- c()
  for (page in pages_to_parse) {
    link <- read_html(RETRY("GET", url = page, pause_min = 5, times = 100))
    if(length(get_last_page(link)) == 0) {
      full_games_list <- c(full_games_list, page)
    } else {
      last_page <- as.numeric(get_last_page(link)) - 1
      full_games_list <- c(full_games_list, str_c(page, '?page=', 0:last_page))
    }
  }
  games <- c()
  
  # Надо бы добавить проверку на вхождение игры в список перед сохранением
  for (page in full_games_list) {
    print(page)
    games <- c(games, get_links(read_html(RETRY("GET", url = page, pause_min = 5, times = 100))))
    print(length(games))
  }
    ## Получаем полные ссылки на страницы с играми
  games_full <- str_remove(paste("https://www.metacritic.com", unique(games)), " ")
  games_full
}
# Updating the base (also used to update the base inside parse_games function after the first run) 
update_base <- function(base, platform) {
  e <- 0
  for (game in setdiff(platform, base[ , "permalink"])) {
    #print(game)
    if (!GET(game)$status %in% c(404, 500) & !is_empty(GET(game)$content)) {
      game_page <- read_html(RETRY("GET", url = game, pause_min = 2, times = 15))
      Title <- get_title(game_page)
      Platform <- get_platform(game_page)
      Developer <- if(is_empty(get_developer(game_page))) {"NA"}
      else {get_developer(game_page)}
      Publisher <- if(is_empty(get_publisher(game_page))) {"NA"}
      else {get_publisher(game_page)}
      ReleaseDate <- get_releasedate(game_page)
      UserScore <- if(is_empty(get_userscore(game_page))) {"NA"}
      else {get_userscore(game_page)}
      UserReviews <- if(is_empty(get_userreviews(game_page))) {"NA"}
      else {get_userreviews(game_page)}
      MetaScore <- if(is_empty(get_metascore(game_page))) {"NA"}
      else {get_metascore(game_page)}
      CriticReviews <- get_criticreviews(game_page)
      Genres <- paste(get_genres(game_page), collapse = ", ")
      permalink <- game
      game_info <- matrix(cbind(Title, Platform, Developer, Publisher, ReleaseDate, 
                                UserScore, UserReviews, MetaScore, CriticReviews, Genres, permalink), ncol = 11)
      base <- rbind(base, game_info)
      # if (nrow(base) %% 100 == 0) {
      #   cat(nrow(base), "games parsed.", e, "games skipped.", length(platform) - (nrow(base) + e), "games remained\n")}
    } else {
      e <- e + 1
      cat("Can't reach the page:", game, "\n")
      next}
  }
  if (nrow(base) == length(platform)) {print("Base complete!")} 
  else {cat("There were", e, "error(s) during the attempt. Please check if the links above are reachable\n")}
  # rm(list = c("Title", "Platform", "Developer", "Publisher", "ReleaseDate",
  #             "UserScore", "UserReviews", "MetaScore", "CriticReviews", "Genres", "permalink"))
  return(base)
}
# Getting the base + trying to update skipped games automatically
parse_games <- function(platform) {
  base <- matrix(, ncol = 11, nrow = 0)
  colnames(base) <- c("Title", "Platform", "Developer", "Publisher", "ReleaseDate",
                      "UserScore", "UserReviews", "MetaScore", "CriticReviews",
                      "Genres", "permalink")
  e <- 0
  for (game in setdiff(platform, base[ , "permalink"])) {
    #print(game)
    if (!GET(game)$status %in% c(404, 500) & !is_empty(GET(game)$content)) {
      game_page <- read_html(RETRY("GET", url = game, pause_min = 2, times = 15))
      Title <- get_title(game_page)
      Platform <- get_platform(game_page)
      Developer <- if(is_empty(get_developer(game_page))) {"NA"}
      else {get_developer(game_page)}
      Publisher <- if(is_empty(get_publisher(game_page))) {"NA"}
      else {get_publisher(game_page)}
      ReleaseDate <- get_releasedate(game_page)
      UserScore <- if(is_empty(get_userscore(game_page))) {"NA"}
      else {get_userscore(game_page)}
      UserReviews <- if(is_empty(get_userreviews(game_page))) {"NA"}
      else {get_userreviews(game_page)}
      MetaScore <- if(is_empty(get_metascore(game_page))) {"NA"}
      else {get_metascore(game_page)}
      CriticReviews <- get_criticreviews(game_page)
      Genres <- paste(get_genres(game_page), collapse = ", ")
      permalink <- game
      game_info <- matrix(cbind(Title, Platform, Developer, Publisher, ReleaseDate, 
                                UserScore, UserReviews, MetaScore, CriticReviews, Genres, permalink), ncol = 11)
      base <- rbind(base, game_info)
      if (nrow(base) %% 100 == 0) {
        cat(nrow(base), "games parsed.", e, "game(s) skipped.", length(platform) - (nrow(base) + e), "games remained\n")}
    } else {
      e <- e + 1
      # cat("The page", game, "is either empty or not found. Total errors in this run:", e, "\n")
      next}
  }
  base
  cat("Trying to parse", e, "missed games...\n")
  print("Resetting errors' counter")
  update_base(base, platform)
  base
}

## Creating the initial links base

## The full base is ~90k rows, so I would strictly recommend NOT TO PARSE IT AT ONE ATTEMPT
## However, you are free to split it in bunches of 5k rows without initial split by platform, if you wish
# platforms <- c("ps", "ps2", "ps3", "ps4", "psp", "vita",
#               "xbox", "xbox360", "xboxone",
#               "n64", "gamecube", "gba", "ds", "3ds", "wii", "wii-u", "switch",
#               "pc",
#               "dreamcast",
#               "ios")

playstation <- c("ps", "ps2", "ps3", "ps4", "psp", "vita")
microsoft <- c("xbox", "xbox360", "xboxone")
nintendo <- c("n64", "gamecube", "gba", "ds", "3ds", "wii", "wii-u", "switch")
pc <- c("pc")
other <- c("dreamcast", "ios")

# Getting base links by platform
# oh, and, well, it will collect links for announced but not yet released games
playstation_platforms <- links_base(playstation)
xbox_platforms <- links_base(microsoft)
nintendo_platforms <- links_base(nintendo)
pc_games <- links_base(pc)
other_platforms <- links_base(other)

# We need to check what's each links base size to split them properly
length(playstation_platforms)
length(xbox_platforms)
length(nintendo_platforms)
length(pc_games)
length(other_platforms)

## We need to clean out games called "***", that does not have actual link and refer to PC games list instead
## If you know something about the game please contact me (search enginges fail to find anything called *** for an obvious reason) 
pc_games <- pc_games[!pc_games %in% c("https://www.metacritic.com/game/pc")]

all_platforms <- c(playstation_platforms, xbox_platforms, nintendo_platforms, pc_games, other_platforms)

# As there are 50k PC games overall, we need to parse it by chunks 
pc_base_part1 <- parse_games(pc_games[1:5000])
pc_base_part2 <- parse_games(pc_games[5001:10000])
pc_base_part3 <- parse_games(pc_games[10001:15000])
pc_base_part4 <- parse_games(pc_games[15001:20000])
pc_base_part5 <- parse_games(pc_games[20001:25000])
pc_base_part6 <- parse_games(pc_games[25001:30000])
pc_base_part7 <- parse_games(pc_games[30001:35000])
pc_base_part8 <- parse_games(pc_games[35001:40000])
pc_base_part9 <- parse_games(pc_games[40001:45000])
pc_base_part10 <- parse_games(pc_games[45001:50000])
pc_base_part11 <- parse_games(pc_games[50001:length(pc_games)])

playstation_base <- parse_games(playstation_platforms)

xbox_base <- parse_games(xbox_platforms)

nintendo_base_part1 <- parse_games(nintendo_platforms[1:5000])
nintendo_base_part2 <- parse_games(nintendo_platforms[5001:length(nintendo_platforms)])

other_base_part1 <- parse_games(other_platforms[1:5000])
other_base_part2 <- parse_games(other_platforms[5001:length(other_platforms)])

## Horay! Let's get the complete PC base
## Это указание на удаление индекса надо будет убрать после соединения, битая ссылка убирается раньше
pc_base <- rbind(pc_base_part1[-29, 1:11], pc_base_part2, pc_base_part3, pc_base_part4, pc_base_part5, 
                 pc_base_part6, pc_base_part7, pc_base_part8, pc_base_part9, pc_base_part10,
                 pc_base_part11)

## And for other platforms as well
nintendo_base <- rbind(nintendo_base_part1, nintendo_base_part2)

other_base <- rbind(other_base_part1, other_base_part2)

## Finally! Let's get the complete (well, you know, we can collect every single review as well, but please NO) base
complete_base <- rbind(pc_base, playstation_base, xbox_base, nintendo_base, other_base)
complete_base <- update_base(complete_base, all_platforms)

write_tsv(complete_base, "complete_base.csv")

## Delete all info that is not needed anymore
rm(list = setdiff(ls(), c("complete_base", "all_platforms")))