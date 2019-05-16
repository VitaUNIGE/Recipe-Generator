#EXTRACTION OF RECIPES PARAMETERS
library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     


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

#Completing the dataframe with all useful elements
Recipe_dataframe$Title <- titles
Recipe_dataframe$Recipe <- recipes
Recipe_dataframe$Ingredients <- ingredients
Recipe_dataframe$Calories <-calories
Recipe_dataframe$cooking_time <- cooking_time

#Extracting ingredients list based on keywords  
ing_indices1 = grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices2 = grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE) 
ing_indices3 = grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices4 = grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices5 = grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)

#Reorganizing all data relating to specific ingredients into matrices
chicken_frame = matrix(ncol = 5, nrow = length(ing_indices1))
for(i in 1:length(ing_indices1)){
  
  chicken_frame[,1] = ingredients [grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  chicken_frame[,2] =  recipes [grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  chicken_frame[,3] =  calories [grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  chicken_frame[,4] =  cooking_time [grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  chicken_frame[,5] =  titles [grep("Chicken", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(chicken_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

salmon_frame = matrix(ncol = 5, nrow = length(ing_indices2))
for(i in 1:length(ing_indices2)){
  
  salmon_frame[,1] = ingredients [grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  salmon_frame[,2] =  recipes [grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  salmon_frame[,3] =  calories [grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  salmon_frame[,4] =  cooking_time [grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  salmon_frame[,5] =  titles [grep("Salmon", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(salmon_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

milk_frame = matrix(ncol = 5, nrow = length(ing_indices3))
for(i in 1:length(ing_indices3)){
  
  milk_frame[,1] = ingredients [grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  milk_frame[,2] =  recipes [grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  milk_frame[,3] =  calories [grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  milk_frame[,4] =  cooking_time [grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  milk_frame[,5] =  titles [grep("Milk", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(milk_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

shrimp_frame = matrix(ncol = 5, nrow = length(ing_indices4))
for(i in 1:length(ing_indices4)){
  
  shrimp_frame[,1] = ingredients [grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  shrimp_frame[,2] =  recipes [grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  shrimp_frame[,3] =  calories [grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  shrimp_frame[,4] =  cooking_time [grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  shrimp_frame[,5] =  titles [grep("Shrimp", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(shrimp_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}


pork_frame = matrix(ncol = 5, nrow = length(ing_indices5))
for(i in 1:length(ing_indices5)){
  
  pork_frame[,1] = ingredients [grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pork_frame[,2] =  recipes [grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pork_frame[,3] =  calories [grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pork_frame[,4] =  cooking_time [grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pork_frame[,5] =  titles [grep("Pork", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(pork_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}



vec_ing = c("Chicken", "Salmon", "Milk", "Shrimp", "Pork")


#CREATING THE APP


library(shiny)



# Frontend

ui = fluidPage(
  
  
  titlePanel("Recipe Generator"),
  
  
  sidebarPanel(
    checkboxGroupInput("ingredients_", label = h3("Ingredient"), 
                       choices = vec_ing),
    selected = "Salmon"),
  
  
  conditionalPanel(
    condition = "input.ingredients_ == 'Chicken'",
    selectInput("chickenrecipe", "Chicken Recipes", choices = chicken_frame[,5]
    )
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Salmon'",
    selectInput("salmonrecipe", "Salmon Recipes", choices = salmon_frame[,5])
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Milk'",
    selectInput("milkrecipe", "Milk Recipes", choices = milk_frame[,5])
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Shrimp'",
    selectInput("shrimprecipe", "Shrimp Recipes", choices = shrimp_frame[,5])
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Pork'",
    selectInput("porkrecipe", "Pork Recipes", choices = pork_frame[,5])
    
    
    
  ),
  
  mainPanel(
    verbatimTextOutput("chickenrecipe_r"),
    verbatimTextOutput("chickenrecipe_i"),
    verbatimTextOutput("chickenrecipe_c"),
    verbatimTextOutput("chickenrecipe_t"),
    
    verbatimTextOutput("salmonrecipe_r"),
    verbatimTextOutput("salmonrecipe_i"),
    verbatimTextOutput("salmonrecipe_c"),
    verbatimTextOutput("salmonrecipe_t"),
    
    verbatimTextOutput("milkrecipe_r"),
    verbatimTextOutput("milkrecipe_i"),
    verbatimTextOutput("milkrecipe_c"),
    verbatimTextOutput("milkrecipe_t"),
    
    verbatimTextOutput("shrimprecipe_r"),
    verbatimTextOutput("shrimprecipe_i"),
    verbatimTextOutput("shrimprecipe_c"),
    verbatimTextOutput("shrimprecipe_t"),
    
    verbatimTextOutput("porkrecipe_r"),
    verbatimTextOutput("porkrecipe_i"),
    verbatimTextOutput("porkrecipe_c"),
    verbatimTextOutput("porkrecipe_t")
  )
)







# Server


server = function(input, output){
  
  
  
  output$milkrecipe_i = renderPrint({
    
    
    x = match(input$milkrecipe, milk_frame[,5])
    
    if(input$milkrecipe == (milk_frame[,5])[x] && input$ingredients_ == "Milk"){
      
      milk_frame[x,1]
      
    }
    
    
  })
  output$milkrecipe_r = renderPrint({
    
    
    x = match(input$milkrecipe, milk_frame[,5])
    
    if(input$milkrecipe == (milk_frame[,5])[x] && input$ingredients_ == "Milk"){
      
      milk_frame[x,2]
      
    }
    
  })
  
  output$milkrecipe_c = renderPrint({
    
    
    x = match(input$milkrecipe, milk_frame[,5])
    
    if(input$milkrecipe == (milk_frame[,5])[x] && input$ingredients_ == "Milk"){
      
      milk_frame[x,3]
      
      
    }
    
  })
  
  output$milkrecipe_t = renderPrint({
    
    
    x = match(input$milkrecipe, milk_frame[,5])
    
    if(input$milkrecipe == (milk_frame[,5])[x] && input$ingredients_ == "Milk"){
      
      milk_frame[x,4]
      
    }
    
  })
  

  
  output$chickenrecipe_i = renderPrint({
    
    
    x = match(input$chickenrecipe, chicken_frame[,5])
    
    if(input$chickenrecipe == (chicken_frame[,5])[x] && input$ingredients_ == "Chicken"){
      
      chicken_frame[x,1]
      
    }
    
    
  })
  output$chickenrecipe_r = renderPrint({
    
    
    x = match(input$chickenrecipe, chicken_frame[,5])
    
    if(input$chickenrecipe == (chicken_frame[,5])[x] && input$ingredients_ == "Chicken"){
      
      chicken_frame[x,2]
      
    }
    
  })
  
  output$chickenrecipe_c = renderPrint({
    
    
    x = match(input$chickenrecipe, chicken_frame[,5])
    
    if(input$chickenrecipe == (chicken_frame[,5])[x] && input$ingredients_ == "Chicken"){
      
      chicken_frame[x,3]
      
    
    }
    
  })
  
  output$chickenrecipe_t = renderPrint({
    
    
    x = match(input$chickenrecipe, chicken_frame[,5])
    
    if(input$chickenrecipe == (chicken_frame[,5])[x] && input$ingredients_ == "Chicken"){
      
      chicken_frame[x,4]
      
    }
    
  })
  
  
  
  output$salmonrecipe_r = renderPrint({
    
    
    x = match(input$salmonrecipe, salmon_frame[,5])
    
    if(input$salmonrecipe == (salmon_frame[,5])[x] && input$ingredients_ == "Salmon"){
      
      salmon_frame[x,2]
      
    }
    
    
  })
  output$salmonrecipe_i = renderPrint({
    
    
    x = match(input$salmonrecipe, salmon_frame[,5])
    
    if(input$salmonrecipe == (salmon_frame[,5])[x] && input$ingredients_ == "Salmon"){
      
      salmon_frame[x,1]
      
    }
    
  })
  
  output$salmonrecipe_c = renderPrint({
    
    
    x = match(input$salmonrecipe, salmon_frame[,5])
    
    if(input$salmonrecipe == (salmon_frame[,5])[x] && input$ingredients_ == "Salmon"){
      
      salmon_frame[x,3]
    }
    
  })
  
  output$salmonrecipe_t = renderPrint({
    
    
    x = match(input$salmonrecipe, salmon_frame[,5])
    
    if(input$salmonrecipe == (salmon_frame[,5])[x] && input$ingredients_ == "Salmon"){
      
      salmon_frame[x,4]
    }
    
  })
  
  
  
  
  output$shrimprecipe_i = renderPrint({
    
    
    x = match(input$shrimprecipe, shrimp_frame[,5])
    
    if(input$shrimprecipe == (shrimp_frame[,5])[x] && input$ingredients_ == "Shrimp"){
      
      shrimp_frame[x,1]
      
    }
    
    
  })
  output$shrimprecipe_r = renderPrint({
    
    
    x = match(input$shrimprecipe, shrimp_frame[,5])
    
    if(input$shrimprecipe == (shrimp_frame[,5])[x] && input$ingredients_ == "Shrimp"){
      
      shrimp_frame[x,2]
      
    }
    
  })
  
  output$shrimprecipe_c = renderPrint({
    
    
    x = match(input$shrimprecipe, shrimp_frame[,5])
    
    if(input$shrimprecipe == (shrimp_frame[,5])[x] && input$ingredients_ == "Shrimp"){
      
      shrimp_frame[x,3]
    }
    
  })
  
  output$shrimprecipe_t = renderPrint({
    
    x = match(input$shrimprecipe, shrimp_frame[,5])
    
    if(input$shrimprecipe == (shrimp_frame[,5])[x] && input$ingredients_ == "Shrimp"){
      
      shrimp_frame[x,4]
    }
    
  })
  
  
  
  
  output$porkrecipe_r = renderPrint({
    
    
    x = match(input$porkrecipe, pork_frame[,5])
    
    if(input$porkrecipe == (pork_frame[,5])[x] && input$ingredients_ == "Pork"){
      
      pork_frame[x,2]
      
    }
    
    
  })
  output$porkrecipe_i = renderPrint({
    
    
    x = match(input$porkrecipe, pork_frame[,5])
    
    if(input$porkrecipe == (pork_frame[,5])[x] && input$ingredients_ == "Pork"){
      
      pork_frame[x,1]
      
    }
    
  })
  
  output$porkrecipe_c = renderPrint({
    
    
    x = match(input$porkrecipe, pork_frame[,5])
    
    if(input$porkrecipe == (pork_frame[,5])[x] && input$ingredients_ == "Pork"){
      
      
      pork_frame[x,3]
    }
    
  })
  
  output$porkrecipe_t = renderPrint({
    
    
    x = match(input$porkrecipe, pork_frame[,5])
    
    if(input$porkrecipe == (pork_frame[,5])[x] && input$ingredients_ == "Pork"){
      
      
      pork_frame[x,4]
    }
    
  })
  
  
  
  

  
}

shinyApp(ui = ui, server = server)

