#EXTRACTION OF RECIPES PARAMETERS
library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)     


# Loading urls for all recipe pages. Urls is initialized as empty vector to prevent repeated entries across runs
urls <- c()
for (t in 1:4) {
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
ing_indices6 = grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices7 = grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices8 = grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices9 = grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices10 = grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices11 = grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices12 = grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices13 = grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices14 = grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices15 = grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices16 = grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)
ing_indices17 = grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)


#Reorganizing all data relating to specific ingredients into matrices, more specifically the recipes,
#the complete list of ingredients related to them, the calories, the cooking time and the titles of recipes
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

cheese_frame = matrix(ncol = 5, nrow = length(ing_indices6))
for(i in 1:length(ing_indices6)){
  
  cheese_frame[,1] = ingredients [grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  cheese_frame[,2] =  recipes [grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  cheese_frame[,3] =  calories [grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  cheese_frame[,4] =  cooking_time [grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  cheese_frame[,5] =  titles [grep("Cheese", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(cheese_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

egg_frame = matrix(ncol = 5, nrow = length(ing_indices7))
for(i in 1:length(ing_indices7)){
  
  egg_frame[,1] = ingredients [grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  egg_frame[,2] =  recipes [grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  egg_frame[,3] =  calories [grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  egg_frame[,4] =  cooking_time [grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  egg_frame[,5] =  titles [grep("Egg", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(egg_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

beef_frame = matrix(ncol = 5, nrow = length(ing_indices8))
for(i in 1:length(ing_indices8)){
  
  beef_frame[,1] = ingredients [grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  beef_frame[,2] =  recipes [grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  beef_frame[,3] =  calories [grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  beef_frame[,4] =  cooking_time [grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  beef_frame[,5] =  titles [grep("Beef", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(beef_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

turkey_frame = matrix(ncol = 5, nrow = length(ing_indices9))
for(i in 1:length(ing_indices9)){
  
  turkey_frame[,1] = ingredients [grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  turkey_frame[,2] =  recipes [grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  turkey_frame[,3] =  calories [grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  turkey_frame[,4] =  cooking_time [grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  turkey_frame[,5] =  titles [grep("Turkey", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(turkey_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

pasta_frame = matrix(ncol = 5, nrow = length(ing_indices10))
for(i in 1:length(ing_indices10)){
  
  pasta_frame[,1] = ingredients [grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pasta_frame[,2] =  recipes [grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pasta_frame[,3] =  calories [grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pasta_frame[,4] =  cooking_time [grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  pasta_frame[,5] =  titles [grep("Pasta", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(pasta_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}
rice_frame = matrix(ncol = 5, nrow = length(ing_indices11))
for(i in 1:length(ing_indices11)){
  
  rice_frame[,1] = ingredients [grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  rice_frame[,2] =  recipes [grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  rice_frame[,3] =  calories [grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  rice_frame[,4] =  cooking_time [grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  rice_frame[,5] =  titles [grep("Rice", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(rice_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}
bread_frame = matrix(ncol = 5, nrow = length(ing_indices12))
for(i in 1:length(ing_indices12)){
  
  bread_frame[,1] = ingredients [grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  bread_frame[,2] =  recipes [grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  bread_frame[,3] =  calories [grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  bread_frame[,4] =  cooking_time [grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  bread_frame[,5] =  titles [grep("Bread", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(bread_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}
onion_frame = matrix(ncol = 5, nrow = length(ing_indices13))
for(i in 1:length(ing_indices13)){
  
  onion_frame[,1] = ingredients [grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  onion_frame[,2] =  recipes [grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  onion_frame[,3] =  calories [grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  onion_frame[,4] =  cooking_time [grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  onion_frame[,5] =  titles [grep("Onion", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(onion_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

potato_frame = matrix(ncol = 5, nrow = length(ing_indices14))
for(i in 1:length(ing_indices14)){
  
  potato_frame[,1] = ingredients [grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  potato_frame[,2] =  recipes [grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  potato_frame[,3] =  calories [grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  potato_frame[,4] =  cooking_time [grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  potato_frame[,5] =  titles [grep("Potato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(potato_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

tomato_frame = matrix(ncol = 5, nrow = length(ing_indices15))
for(i in 1:length(ing_indices15)){
  
  tomato_frame[,1] = ingredients [grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  tomato_frame[,2] =  recipes [grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  tomato_frame[,3] =  calories [grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  tomato_frame[,4] =  cooking_time [grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  tomato_frame[,5] =  titles [grep("Tomato", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(tomato_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}

orange_frame = matrix(ncol = 5, nrow = length(ing_indices16))
for(i in 1:length(ing_indices16)){
  
  orange_frame[,1] = ingredients [grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  orange_frame[,2] =  recipes [grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  orange_frame[,3] =  calories [grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  orange_frame[,4] =  cooking_time [grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  orange_frame[,5] =  titles [grep("Orange", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(orange_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}
apple_frame = matrix(ncol = 5, nrow = length(ing_indices17))
for(i in 1:length(ing_indices17)){
  
  apple_frame[,1] = ingredients [grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  apple_frame[,2] =  recipes [grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  apple_frame[,3] =  calories [grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  apple_frame[,4] =  cooking_time [grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  apple_frame[,5] =  titles [grep("Apple", Recipe_dataframe$Ingredients, ignore.case = TRUE)]
  colnames(apple_frame) <-c ("Ingredidents", "Recipe", "calories", "Cooking Time", "Title")
}


#Putting all the ingredients into a vector for easy display in the app
vec_ing = c("Egg", "Milk", "Cheese","Pasta", "Rice", "Bread", "Chicken", "Pork", "Beef", "Salmon", "Shrimp", "Turkey", "Onion", "Potato", "Tomato", "Orange", "Apple")


#CREATING THE APP


library(shiny)



# Frontend
#In this section the selection of ingredients is linked to a scroll down menu offering a choice of recipes referred to as choices

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
  conditionalPanel(
    condition = "input.ingredients_ == 'Cheese'",
    selectInput("cheeserecipe", "Cheese Recipes", choices = cheese_frame[,5])
    
  ),
  
  conditionalPanel(
    condition = "input.ingredients_ == 'Egg'",
    selectInput("eggrecipe", "Egg Recipes", choices = egg_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Beef'",
    selectInput("beefrecipe", "Beef Recipes", choices = beef_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Turkey'",
    selectInput("turkeyrecipe", "Turkey Recipes", choices = turkey_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Pasta'",
    selectInput("pastarecipe", "Pasta Recipes", choices = pasta_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Rice'",
    selectInput("ricerecipe", "Rice Recipes", choices = rice_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Bread'",
    selectInput("breadrecipe", "Bread Recipes", choices = bread_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Onion'",
    selectInput("onionrecipe", "Onion Recipes", choices = onion_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Potato'",
    selectInput("potatorecipe", "Potato Recipes", choices = potato_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Tomato'",
    selectInput("tomatorecipe", "Tomato Recipes", choices = tomato_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Orange'",
    selectInput("orangerecipe", "Orange Recipes", choices = orange_frame[,5])
    
  ),
  conditionalPanel(
    condition = "input.ingredients_ == 'Apple'",
    selectInput("applerecipe", "Apple Recipes", choices = apple_frame[,5])
    
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
    verbatimTextOutput("porkrecipe_t"),
    
    verbatimTextOutput("cheeserecipe_r"),
    verbatimTextOutput("cheeserecipe_i"),
    verbatimTextOutput("cheeserecipe_c"),
    verbatimTextOutput("cheeserecipe_t"),
    
    verbatimTextOutput("eggrecipe_r"),
    verbatimTextOutput("eggrecipe_i"),
    verbatimTextOutput("eggrecipe_c"),
    verbatimTextOutput("eggrecipe_t"),
    
    verbatimTextOutput("beefrecipe_r"),
    verbatimTextOutput("beefrecipe_i"),
    verbatimTextOutput("beefrecipe_c"),
    verbatimTextOutput("beefrecipe_t"),
    
    verbatimTextOutput("turkeyrecipe_r"),
    verbatimTextOutput("turkeyrecipe_i"),
    verbatimTextOutput("turkeyrecipe_c"),
    verbatimTextOutput("turkeyrecipe_t"),
    
    verbatimTextOutput("pastarecipe_r"),
    verbatimTextOutput("pastarecipe_i"),
    verbatimTextOutput("pastarecipe_c"),
    verbatimTextOutput("pastarecipe_t"),
    
    verbatimTextOutput("ricerecipe_r"),
    verbatimTextOutput("ricerecipe_i"),
    verbatimTextOutput("ricerecipe_c"),
    verbatimTextOutput("ricerecipe_t"),
    
    verbatimTextOutput("breadrecipe_r"),
    verbatimTextOutput("breadrecipe_i"),
    verbatimTextOutput("breadrecipe_c"),
    verbatimTextOutput("breadrecipe_t"),
    
    verbatimTextOutput("onionrecipe_r"),
    verbatimTextOutput("onionrecipe_i"),
    verbatimTextOutput("onionrecipe_c"),
    verbatimTextOutput("onionrecipe_t"),
    
    verbatimTextOutput("potatorecipe_r"),
    verbatimTextOutput("potatorecipe_i"),
    verbatimTextOutput("potatorecipe_c"),
    verbatimTextOutput("potatorecipe_t"),
    
    verbatimTextOutput("tomatorecipe_r"),
    verbatimTextOutput("tomatorecipe_i"),
    verbatimTextOutput("tomatorecipe_c"),
    verbatimTextOutput("tomatorecipe_t"),
    
    verbatimTextOutput("orangerecipe_r"),
    verbatimTextOutput("orangerecipe_i"),
    verbatimTextOutput("orangerecipe_c"),
    verbatimTextOutput("orangerecipe_t"),
    
    verbatimTextOutput("applerecipe_r"),
    verbatimTextOutput("applerecipe_i"),
    verbatimTextOutput("applerecipe_c"),
    verbatimTextOutput("applerecipe_t")
  )
)







# Server
#This part of the code allows the app to output each ingredients parameter once the recipe is selected in the scroll down menu

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
  
  
  
  output$cheeserecipe_r = renderPrint({
    
    
    x = match(input$cheeserecipe, cheese_frame[,5])
    
    if(input$cheeserecipe == (cheese_frame[,5])[x] && input$ingredients_ == "Cheese"){
      
      cheese_frame[x,2]
      
    }
    
    
  })
  output$cheeserecipe_i = renderPrint({
    
    
    x = match(input$cheeserecipe, cheese_frame[,5])
    
    if(input$cheeserecipe == (cheese_frame[,5])[x] && input$ingredients_ == "Cheese"){
      
      cheese_frame[x,1]
      
    }
    
  })
  
  output$cheeserecipe_c = renderPrint({
    
    
    x = match(input$cheeserecipe, cheese_frame[,5])
    
    if(input$cheeserecipe == (cheese_frame[,5])[x] && input$ingredients_ == "Cheese"){
      
      cheese_frame[x,3]
    }
    
  })
  
  output$cheeserecipe_t = renderPrint({
    
    
    x = match(input$cheeserecipe, cheese_frame[,5])
    
    if(input$cheeserecipe == (cheese_frame[,5])[x] && input$ingredients_ == "Cheese"){
      
      cheese_frame[x,4]
    }
    
  }) 
  
  output$eggrecipe_r = renderPrint({
    
    
    x = match(input$eggrecipe, egg_frame[,5])
    
    if(input$eggrecipe == (egg_frame[,5])[x] && input$ingredients_ == "Egg"){
      
      egg_frame[x,2]
      
    }
    
    
  })
  output$eggrecipe_i = renderPrint({
    
    
    x = match(input$eggrecipe, egg_frame[,5])
    
    if(input$eggrecipe == (egg_frame[,5])[x] && input$ingredients_ == "Egg"){
      
      egg_frame[x,1]
      
    }
    
  })
  
  output$eggrecipe_c = renderPrint({
    
    
    x = match(input$eggrecipe, egg_frame[,5])
    
    if(input$eggrecipe == (egg_frame[,5])[x] && input$ingredients_ == "Egg"){
      
      egg_frame[x,3]
    }
    
  })
  
  output$eggrecipe_t = renderPrint({
    
    
    x = match(input$eggrecipe, egg_frame[,5])
    
    if(input$eggrecipe == (egg_frame[,5])[x] && input$ingredients_ == "Egg"){
      
      egg_frame[x,4]
    }
    
  }) 

  
  output$beefrecipe_r = renderPrint({
    
    
    x = match(input$beefrecipe, beef_frame[,5])
    
    if(input$beefrecipe == (beef_frame[,5])[x] && input$ingredients_ == "Beef"){
      
      beef_frame[x,2]
      
    }
    
    
  })
  output$beefrecipe_i = renderPrint({
    
    
    x = match(input$beefrecipe, beef_frame[,5])
    
    if(input$beefrecipe == (beef_frame[,5])[x] && input$ingredients_ == "Beef"){
      
      beef_frame[x,1]
      
    }
    
  })
  
  output$beefrecipe_c = renderPrint({
    
    
    x = match(input$beefrecipe, beef_frame[,5])
    
    if(input$beefrecipe == (beef_frame[,5])[x] && input$ingredients_ == "Beef"){
      
      beef_frame[x,3]
    }
    
  })
  
  output$beefrecipe_t = renderPrint({
    
    
    x = match(input$beefrecipe, beef_frame[,5])
    
    if(input$beefrecipe == (beef_frame[,5])[x] && input$ingredients_ == "Beef"){
      
      beef_frame[x,4]
    }
    
  }) 
  
  
  
  
  output$turkeyrecipe_r = renderPrint({
    
    
    x = match(input$turkeyrecipe, turkey_frame[,5])
    
    if(input$turkeyrecipe == (turkey_frame[,5])[x] && input$ingredients_ == "Turkey"){
      
      turkey_frame[x,2]
      
    }
    
    
  })
  output$turkeyrecipe_i = renderPrint({
    
    
    x = match(input$turkeyrecipe, turkey_frame[,5])
    
    if(input$turkeyrecipe == (turkey_frame[,5])[x] && input$ingredients_ == "Turkey"){
      
      turkey_frame[x,1]
      
    }
    
  })
  
  output$turkeyrecipe_c = renderPrint({
    
    
    x = match(input$turkeyrecipe, turkey_frame[,5])
    
    if(input$turkeyrecipe == (turkey_frame[,5])[x] && input$ingredients_ == "Turkey"){
      
      turkey_frame[x,3]
    }
    
  })
  
  output$turkeyrecipe_t = renderPrint({
    
    
    x = match(input$turkeyrecipe, turkey_frame[,5])
    
    if(input$turkeyrecipe == (turkey_frame[,5])[x] && input$ingredients_ == "Turkey"){
      
      turkey_frame[x,4]
    }
    
  }) 
  
  
  output$pastarecipe_r = renderPrint({
    
    
    x = match(input$pastarecipe, pasta_frame[,5])
    
    if(input$pastarecipe == (pasta_frame[,5])[x] && input$ingredients_ == "Pasta"){
      
      pasta_frame[x,2]
      
    }
    
    
  })
  output$pastarecipe_i = renderPrint({
    
    
    x = match(input$pastarecipe, pasta_frame[,5])
    
    if(input$pastarecipe == (pasta_frame[,5])[x] && input$ingredients_ == "Pasta"){
      
      pasta_frame[x,1]
      
    }
    
  })
  
  output$pastarecipe_c = renderPrint({
    
    
    x = match(input$pastarecipe, pasta_frame[,5])
    
    if(input$pastarecipe == (pasta_frame[,5])[x] && input$ingredients_ == "Pasta"){
      
      pasta_frame[x,3]
    }
    
  })
  
  output$pastarecipe_t = renderPrint({
    
    
    x = match(input$pastarecipe, pasta_frame[,5])
    
    if(input$pastarecipe == (pasta_frame[,5])[x] && input$ingredients_ == "Pasta"){
      
      pasta_frame[x,4]
    }
    
  }) 
  
  
  output$ricerecipe_r = renderPrint({
    
    
    x = match(input$ricerecipe, rice_frame[,5])
    
    if(input$ricerecipe == (rice_frame[,5])[x] && input$ingredients_ == "Rice"){
      
      rice_frame[x,2]
      
    }
    
    
  })
  output$ricerecipe_i = renderPrint({
    
    
    x = match(input$ricerecipe, rice_frame[,5])
    
    if(input$ricerecipe == (rice_frame[,5])[x] && input$ingredients_ == "Rice"){
      
      rice_frame[x,1]
      
    }
    
  })
  
  output$ricerecipe_c = renderPrint({
    
    
    x = match(input$ricerecipe, rice_frame[,5])
    
    if(input$ricerecipe == (rice_frame[,5])[x] && input$ingredients_ == "Rice"){
      
      rice_frame[x,3]
    }
    
  })
  
  output$ricerecipe_t = renderPrint({
    
    
    x = match(input$ricerecipe, rice_frame[,5])
    
    if(input$ricerecipe == (rice_frame[,5])[x] && input$ingredients_ == "Rice"){
      
      rice_frame[x,4]
    }
    
  })
  
  
  output$breadrecipe_r = renderPrint({
    
    
    x = match(input$breadrecipe, bread_frame[,5])
    
    if(input$breadrecipe == (bread_frame[,5])[x] && input$ingredients_ == "Bread"){
      
      bread_frame[x,2]
      
    }
    
    
  })
  output$breadrecipe_i = renderPrint({
    
    
    x = match(input$breadrecipe, bread_frame[,5])
    
    if(input$breadrecipe == (bread_frame[,5])[x] && input$ingredients_ == "Bread"){
      
      bread_frame[x,1]
      
    }
    
  })
  
  output$breadrecipe_c = renderPrint({
    
    
    x = match(input$breadrecipe, bread_frame[,5])
    
    if(input$breadrecipe == (bread_frame[,5])[x] && input$ingredients_ == "Bread"){
      
      bread_frame[x,3]
    }
    
  })
  
  output$breadrecipe_t = renderPrint({
    
    
    x = match(input$breadrecipe, bread_frame[,5])
    
    if(input$breadrecipe == (bread_frame[,5])[x] && input$ingredients_ == "Bread"){
      
      bread_frame[x,4]
    }
    
  })
  
  
  
  output$onionrecipe_r = renderPrint({
    
    
    x = match(input$onionrecipe, onion_frame[,5])
    
    if(input$onionrecipe == (onion_frame[,5])[x] && input$ingredients_ == "Onion"){
      
      onion_frame[x,2]
      
    }
    
    
  })
  output$onionrecipe_i = renderPrint({
    
    
    x = match(input$onionrecipe, onion_frame[,5])
    
    if(input$onionrecipe == (onion_frame[,5])[x] && input$ingredients_ == "Onion"){
      
      onion_frame[x,1]
      
      
    }
    
  })
  
  output$onionrecipe_c = renderPrint({
    
    
    x = match(input$onionrecipe, onion_frame[,5])
    
    if(input$onionrecipe == (onion_frame[,5])[x] && input$ingredients_ == "Onion"){
      
      onion_frame[x,3]
      
    }
    
  })
  
  output$onionrecipe_t = renderPrint({
    
    
    x = match(input$onionrecipe, onion_frame[,5])
    
    if(input$onionrecipe == (onion_frame[,5])[x] && input$ingredients_ == "Onion"){
      
      onion_frame[x,4]
      
    }
    
  })
  
  
  output$potatorecipe_r = renderPrint({
    
    
    x = match(input$potatorecipe, potato_frame[,5])
    
    if(input$potatorecipe == (potato_frame[,5])[x] && input$ingredients_ == "Potato"){
      
      potato_frame[x,2]
      
    }
    
    
  })
  output$potatorecipe_i = renderPrint({
    
    
    x = match(input$potatorecipe, potato_frame[,5])
    
    if(input$potatorecipe == (potato_frame[,5])[x] && input$ingredients_ == "Potato"){
      
      potato_frame[x,1]
      
      
      
    }
    
  })
  
  output$potatorecipe_c = renderPrint({
    
    
    x = match(input$potatorecipe, potato_frame[,5])
    
    if(input$potatorecipe == (potato_frame[,5])[x] && input$ingredients_ == "Potato"){
      
      potato_frame[x,3]
      
      
    }
    
  })
  
  output$potatorecipe_t = renderPrint({
    
    
    x = match(input$potatorecipe, potato_frame[,5])
    
    if(input$potatorecipe == (potato_frame[,5])[x] && input$ingredients_ == "Potato"){
      
      potato_frame[x,4]
      
    }
    
  })
  
  
  
  output$tomatorecipe_r = renderPrint({
    
    
    x = match(input$tomatorecipe, tomato_frame[,5])
    
    if(input$tomatorecipe == (tomato_frame[,5])[x] && input$ingredients_ == "Tomato"){
      
      tomato_frame[x,2]
      
    }
    
    
  })
  output$tomatorecipe_i = renderPrint({
    
    
    x = match(input$tomatorecipe, tomato_frame[,5])
    
    if(input$tomatorecipe == (tomato_frame[,5])[x] && input$ingredients_ == "Tomato"){
      
      tomato_frame[x,1]
      
      
      
    }
    
  })
  
  output$tomatorecipe_c = renderPrint({
    
    
    x = match(input$tomatorecipe, tomato_frame[,5])
    
    if(input$tomatorecipe == (tomato_frame[,5])[x] && input$ingredients_ == "Tomato"){
      
      tomato_frame[x,3]
      
      
    }
    
  })
  
  output$tomatorecipe_t = renderPrint({
    
    
    x = match(input$tomatorecipe, tomato_frame[,5])
    
    if(input$tomatorecipe == (tomato_frame[,5])[x] && input$ingredients_ == "Tomato"){
      
      tomato_frame[x,4]
      
    }
    
  })
  
  output$orangerecipe_r = renderPrint({
    
    
    x = match(input$orangerecipe, orange_frame[,5])
    
    if(input$orangerecipe == (orange_frame[,5])[x] && input$ingredients_ == "Orange"){
      
      orange_frame[x,2]
      
    }
    
    
  })
  output$orangerecipe_i = renderPrint({
    
    
    x = match(input$orangerecipe, orange_frame[,5])
    
    if(input$orangerecipe == (orange_frame[,5])[x] && input$ingredients_ == "Orange"){
      
      orange_frame[x,1]
      
      
      
    }
    
  })
  
  output$orangerecipe_c = renderPrint({
    
    
    x = match(input$orangerecipe, orange_frame[,5])
    
    if(input$orangerecipe == (orange_frame[,5])[x] && input$ingredients_ == "Orange"){
      
      orange_frame[x,3]
      
      
    }
    
  })
  
  output$orangerecipe_t = renderPrint({
    
    
    x = match(input$orangerecipe, orange_frame[,5])
    
    if(input$orangerecipe == (orange_frame[,5])[x] && input$ingredients_ == "Orange"){
      
      orange_frame[x,4]
      
    }
    
  })
  
  output$applerecipe_r = renderPrint({
    
    
    x = match(input$applerecipe, apple_frame[,5])
    
    if(input$applerecipe == (apple_frame[,5])[x] && input$ingredients_ == "Apple"){
      
      apple_frame[x,2]
      
    }
    
    
  })
  output$applerecipe_i = renderPrint({
    
    
    xx = match(input$applerecipe, apple_frame[,5])
    
    if(input$applerecipe == (apple_frame[,5])[x] && input$ingredients_ == "Apple"){
      
      apple_frame[x,1]
      
      
      
    }
    
  })
  
  output$applerecipe_c = renderPrint({
    
    
    x = match(input$applerecipe, apple_frame[,5])
    
    if(input$applerecipe == (apple_frame[,5])[x] && input$ingredients_ == "Apple"){
      
      apple_frame[x,3]
      
      
    }
    
  })
  
  output$applerecipe_t = renderPrint({
    
    
    x = match(input$applerecipe, apple_frame[,5])
    
    if(input$applerecipe == (apple_frame[,5])[x] && input$ingredients_ == "Apple"){
      
      apple_frame[x,4]
      
    }
    
  })
}

#Finally, this runs the app:
shinyApp(ui = ui, server = server)

