library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     
library(lubridate)


webpage_recipes <- read_html("https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=15")  
urls <- html_nodes (webpage_recipes, ".fixed-recipe-card__h3 a") %>%
        html_attr("href")
  
  n = length(recipes)
  recipes = rep(NA, n)
  ingredients = rep(NA, n)
  Images = rep(NA, n)
  Time = rep(NA, n)
  Calories = rep(NA, n)




TITLE_Recipes_data_html <- html_nodes(webpage,".fixed-recipe-card__h3")

TITLE_data <- html_text( Recipes_data_html )


INSTRUCTIONS_data_html <- html_nodes(webpage, "")

INSTRUCTIONS_data <- html_text(INSTRUCTIONS_data_html)