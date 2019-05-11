library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     
library(lubridate)

titles <-c()
recipe <- c()
ingredients <-c()
image <-c()
cooking.time <-c()
calories <-c()

webpage_recipes1 <- read_html("https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=1")  
urls1 <- html_nodes (webpage_recipes1, ".fixed-recipe-card__h3 a") %>%
        html_attr("href")

webpage_recipes2 <- read_html("https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=2")  
urls2 <- html_nodes (webpage_recipes2, ".fixed-recipe-card__h3 a") %>%
  html_attr("href")

webpage_recipes3 <- read_html("https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=3")  
urls3 <- html_nodes (webpage_recipes3, ".fixed-recipe-card__h3 a") %>%
  html_attr("href")

Info1 = c(urls1)
Info2 = c(urls2)
c(Info1,Info2)


Recipe_dataframe <- data.frame (
  WebPg=urls1
)

Recipe_dataframe <- append(Recipe_dataframe, data.frame(WebPg=urls2))


for (i in Recipe_dataframe$WebPg){
  Recipe_webpg <-read_html(i)
  title <- Recipe_webpg %>%
    html_node("Title") %>%
  html_text()
  titles <-append(titles, title)
}

Recipe_dataframe$Title <- titles

  
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