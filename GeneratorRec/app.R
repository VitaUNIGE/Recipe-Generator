library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     
library(lubridate)

get_recipes = function(){
  url <- "https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=15"
  webpage <- read_html(url)
  
  urls <- recipes_on_webpage %>% 
    html_nodes ("div.searchresult a") %>% 
    html_attr("href")
  read_html(urls)
  
}





TITLE_Recipes_data_html <- html_nodes(webpage,".fixed-recipe-card__h3")

TITLE_data <- html_text( Recipes_data_html )


INSTRUCTIONS_data_html <- html_nodes(webpage, "")

INSTRUCTIONS_data <- html_text(INSTRUCTIONS_data_html)