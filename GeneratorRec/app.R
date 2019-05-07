library(shiny)
library(xml2)
library(rvest)
library(ggmap)

get_site_recipes = function() {
  Many_Recipes <- read_html(
    "https://www.cuisineaz.com/"
  )
}

recipes <- Many_Recipes %>% 
  html_nodes(".p10") %>%
  html_text()



