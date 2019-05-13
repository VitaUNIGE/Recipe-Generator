#EXTRACTION OF RECIPES PARAMETERS
library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     
library(lubridate)

# Loading urls for all recipe pages. Urls is initialized as empty vector to prevent repeated entries across runs
urls <- c()
for (t in 1:2) {
  webpage_recipes <-read_html(paste0("https://www.allrecipes.com/recipes/1947/everyday-cooking/quick-and-easy/?page=", t))
  urlst <-html_nodes (webpage_recipes, ".fixed-recipe-card__h3 a") %>%
    html_attr("href")
  urls <- append(urls, urlst)
}

# Creates the dataframe for all recipes
Recipe_dataframe <- c()
Recipe_dataframe <- data.frame (
  WebPg=urls
)

# Creates vectors for recipe parameters to be scraped
titles <-c()
recipes <- c()
ingredients <-c()
cooking_time <-c()
calories <-c()

# Scrapes the recipe parameters
for (webpg in Recipe_dataframe$WebPg){
  
  # Loads the page source code
  Recipe_webpg <-read_html(webpg)
  
  # Scrapes the Title
  title <- Recipe_webpg %>%
    html_node("#recipe-main-content") %>%
  html_text()
  titles <-append(titles, title) 
    
  # Scrapes the recipe instructions
  recipe <- Recipe_webpg %>%
    html_node (".recipe-directions__list")%>%
    html_text()
  recipe_clean <- str_squish(recipe)
  recipes <-append(recipes, recipe_clean)

  # Scrapes the ingredients
  ingredient  <- Recipe_webpg %>%
    html_nodes (".checkList__line") %>%
    html_text()
  ingredient_clean <-str_squish(ingredient)
  ingredients <-append(ingredients, paste(head(ingredient_clean, -3), collapse=";"))
  
  # Scrapes the calories
  calorie <- Recipe_webpg %>%
    html_node(".calorie-count") %>%
    html_text()
  calories <-append(calories, calorie) 
  
  # Scrapes the cooking time
  cooking <- Recipe_webpg %>%
    html_node(".ready-in-time") %>%
    html_text()
  cooking_time <-append(cooking_time, cooking) 
}


Recipe_dataframe$Title <- titles
Recipe_dataframe$Recipe <- recipes
Recipe_dataframe$Ingredients <- ingredients
Recipe_dataframe$Calories <-calories
Recipe_dataframe$cooking_time <- cooking_time

  





research = function(ing1,ing2){
  
  
  if(ing1 == TRUE){
    
    titles_indices = grep("Chicken", titles, ignore.case = TRUE)
    
    for (i in 1:length(titles_indices)){
      
      x[i] = c(titles[titles_indices[i]])
      
      
      
    }
    x
    
  }
  
  if(ing2 == TRUE){
    
    titles_indicesy = grep("Salmon", titles, ignore.case = TRUE)
    
    for (i in 1:length(titles_indices)){
      
      y[i] = c(titles[titles_indicesy[i]])
      
      
      
    }
    y
    
  }
  
  
  
}




library(shiny)



# Frontend

ui = fluidPage(
  
  
  titlePanel("Recipe Generator"),
  
  
  # Sidebar 
  
  sidebarLayout(
    sidebarPanel(
      
      
      
    checkboxInput("ing1", "Chicken", value = FALSE),
    checkboxInput("ing2", "Salmon", value = FALSE),
    checkboxInput("ing3", "Pork", value = FALSE)
    ),
      
      
    
    mainPanel(
      textOutput("Recipes")
      
      
      
      
    )
  )

)







# Server


server = function(input, output){
  
  
  output$Recipes = renderText({
      ing1 = input$ing1
      ing2 = input$ing2
research(ing1, ing2)
  
})
}

shinyApp(ui = ui, server = server)

