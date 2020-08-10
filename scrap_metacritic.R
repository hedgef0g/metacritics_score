#install.packages("tidyverse")  
#install.packages("rvest")    
#install.packages("stringr")   
#install.packages("rebus")     
#install.packages("lubridate")

library(tidyverse)  
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)

url <- 'https://www.metacritic.com/browse/games/score/userscore/all/all/filtered?view=condensed&sort=desc&'

last_page_number <- 163 # define last page manually (probably needs to be automated) = 163

list_of_pages <- str_c(url, 'page=', 0:last_page_number) # metacritic's filtered results output starts from page 0

get_titles <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.details h3') %>%      
    html_text() %>% 
    # Trim additional white space, remove line breaks and "Metascore" text
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[\r\n]", "") %>%
    str_remove_all("Metascore") %>%
    # Convert the list into a vector
    unlist()
}

get_platforms <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.platform') %>%      
    html_text() %>% 
    # Trim additional white space
    str_remove_all("Platform:") %>%
    str_trim() %>%
    str_squish() %>%
    # Convert the list into a vector
    unlist()
}

get_scores <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.metascore_anchor') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%
    str_squish() %>%
    # Convert the list into a vector
    unlist()
}

get_release_dates <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.title_bump span') %>%
    mdy()
}

meta_base <- matrix( , ncol = 5, nrow = 0) #tibble(titles, platforms, user_scores,  critic_scores, release_dates)

clean_titles <- c()
clean_platforms <- c()
clean_users_scores <- c()
clean_critics_scores <- c()
clean_release_dates <- c()

for (page in list_of_pages) {
  print(page)
  
  if(str_detect(read_html(page), "No games found.")) {next} # some pages are broken and just tell "No games found"
  else  {
    titles <- get_titles(read_html(page, options = "RECOVER"))
    clean_titles <- cbind(titles[titles != ""]) # clean out empty entries
    
    platforms <- get_platforms(read_html(page, options = "RECOVER"))
    clean_platforms <- cbind(trimws(platforms[2:(length(clean_titles)+1)])) # remove extra spaces
    
    scores <- as.numeric(get_scores(read_html(page, options = "RECOVER")))
    clean_users_scores <- cbind(scores[c(TRUE, FALSE)]) # get user scores
    clean_critics_scores <- cbind(scores[c(FALSE, TRUE)]) # get critic scores
    
    release_dates <- get_release_dates(read_html(page, options = "RECOVER"))
    clean_release_dates <- cbind(as.character(release_dates[!is.na(release_dates)])) # get release date
    
    page_matrix <- matrix(cbind(clean_titles, clean_platforms, clean_users_scores, clean_critics_scores, clean_release_dates), ncol = 5)
    
    meta_base <- rbind(meta_base, page_matrix)
  } 
}

colnames(meta_base) <- c("Title", "Platform", "UsersScore", "CriticsScore", "ReleaseDate")
meta_base <- as_tibble(meta_base)

write_tsv(meta_base, "./metacritics_score.csv")
rm(list = ls())