install.packages(tidyverse)  
install.packages(rvest)    
install.packages(stringr)   
install.packages(rebus)     
install.packages(lubridate)

library(tidyverse)  
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)

url <- 'https://www.metacritic.com/browse/games/score/userscore/all/all/filtered?view=condensed&sort=desc&page=0'

get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.page last_page') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data))] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

first_page <- read_html(url)
latest_page_number <- get_last_page(first_page)