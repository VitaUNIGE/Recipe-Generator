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

  
titles_indices1 = grep("Chicken", titles, ignore.case = TRUE)
titles_indices2 = grep("Salmon", titles, ignore.case = TRUE) 
titles_indices3 = grep("Thuna", titles, ignore.case = TRUE)
titles_indices4 = grep("Shrimp", titles, ignore.case = TRUE)
chicken_r = c()
salmon_r = c()
chicken_recipe = c()
salmon_recipe = c()

vec_ing = c("Chicken", "Salmon")
  
for (i in 1:length(titles_indices1)){
  
  chicken_r[i] = c(titles[titles_indices1[i]])
  chicken_recipe[i] = c(recipes[titles_indices1[i]])
  
  
  
}

for (i in 1:length(titles_indices2)){
  
  salmon_r[i] = c(titles[titles_indices2[i]])
  salmon_recipe[i] = c(recipes[titles_indices2[i]])
  
  
  
}
for (i in 1:length(titles_indices3)){
  
  thuna_r[i] = c(titles[titles_indices3[i]])
  
  
  
  
}
for (i in 1:length(titles_indices4)){
  
  shrimp_r[i] = c(titles[titles_indices4[i]])
  
  
  
  
}



  
  



library(shiny)



# Frontend

ui = fluidPage(
  
  
  titlePanel("Recipe Generator"),
  
  sidebarPanel(
      
    selectInput("ingredients_", "Ingredient",
                vec_ing
    ),

      conditionalPanel(
      condition = "input.ingredients_ == 'Chicken'",
      selectInput(
        "chickenrecipe", "Chicken Recipes", 
        choices = chicken_r
      )

   
    ),
    conditionalPanel(
      condition = "input.ingredients_ == 'Salmon'",
      selectInput("salmonrecipe", "Salmon Recipes", choices = salmon_r)
    )
     
  ),
  mainPanel(
     verbatimTextOutput("chickenrecipe"),
    verbatimTextOutput("salmonrecipe")
      
  )

)

      
    
 



# Server


server = function(input, output){
  
  

  
  output$chickenrecipe = renderText({
    
    
    x = match(input$chickenrecipe, chicken_r)
    
    if(input$chickenrecipe == chicken_r[x] && input$ingredients_ == "Chicken"){
      
 chicken_recipe[x]
    }
    
    
    
    
   
  })
  
  output$salmonrecipe = renderText({
    
    
    x = match(input$salmonrecipe, salmon_r)
    
    if(input$salmonrecipe == salmon_r[x] && input$ingredients_ == "Salmon"){
      
      salmon_recipe[x]
    }
    
    
    
    
    
  })

  

  
  
}

shinyApp(ui = ui, server = server)

