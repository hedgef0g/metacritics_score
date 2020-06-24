install.packages("tidyverse")  
install.packages("rvest")    
install.packages("stringr")   
install.packages("rebus")     
install.packages("lubridate")

library(tidyverse)  
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)

url <- 'https://www.metacritic.com/browse/games/score/userscore/all/all/filtered?view=condensed&sort=desc&'

first_page <- read_html(url)
latest_page_number <- 162 # define last page manually (probably needs to be automated)

list_of_pages <- str_c(url, 'page=', 0:latest_page_number) # metacritic's filtered results output starts from page 0
# head(list_of_pages) # check if the resulted URLs are valid (probably needs to be automated)

