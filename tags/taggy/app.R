library(shiny)
library(DT)
require(shinyWidgets)

# Custom CSS for the checkboxGroupButtons
# custom_css <- "
# .checkboxGroupButtons .btn {
#   background-color: red; /* Red when not clicked */
#   color: white;
# }
# 
# .checkboxGroupButtons .btn.active {
#   background-color: green; /* Green when clicked */
#   color: white;
# }
# 
# .checkboxGroupButtons .btn:hover {
#   background-color: #555; /* Darker on hover */
# }
# "

custom_css <- "
.checkboxGroupButtons {
  border: 2px solid blue !important; /* Add border to verify loading */
}
"


# List of tags
# recipe_tags <- c("Favorite",
#                  "Southern", "Indian", "Chinese", "Vietnamese", "Thai", "Korean", "French", "Cajun", "Middle eastern", "Mexican", "Italian", "Mediterranian",
#                  "Easy", "Complicated", "Kid-friendly", "Requires prep work", "Do-ahead",
#                  "Breakfast", "Dessert", "Dressing", "Appetizer", "Sauce/condiment", "Ingredient",
#                  "Noodles", "Salad", "Soup/stew", "Pastry",
#                  "Side dish", "Main course", "Meal")


rt_meta <- c("Favorite", "Made previously", "Kid-friendly",  "Large yield", "Good", "Great", "Meh")
rt_time <- c("Requires prep", "Do-ahead", "<20 minutes", "20-40 minutes", "40-80 minutes", "80-120 minutes", "2+ hours")
rt_cuisine <- c("American", "Southern", "Chinese", "Vietnamese", "Thai", "Korean", "Japanese",
               "Mexican", "Latin", "Cajun","French",  "Italian", "Mediterranian","Middle eastern","Indian")
rt_type <- c("Dressing",  "Sauce/condiment", "Ingredient", "Salad", "Soup/stew",
             "Meal","Side dish", "Main course",  "Appetizer","Breakfast", "Dessert")
rt_method <- c("Grilling", "Smoking", "Sous vide", "Deep fry", "Baking", "Stir fry", "Pressure cooker", "Oven/baking", "Stove top")

recipe_tags <- c(rt_meta, rt_time, rt_cuisine, rt_type, rt_method)

# Function to pad labels with spaces to ensure consistent length
pad_labels <- function(labels, length = 30) {
  sapply(labels, function(x) sprintf(paste0("%-", length, "s"), x))
}

# Pad labels
padded_tags <- pad_labels(recipe_tags, length = 30)


recipes_naive <- read.csv("../../data/recipes_in.csv")%>%
  mutate(tag = "") %>% select(-Type, -Cuisine, -Selected)%>%
  mutate(tagged = ifelse(tag=="", "No", "Yes"))



ingredients_static <- read.csv("../../data/ingredients.csv") %>%
  mutate(rn = 1:n()) %>% tibble::as_tibble()
filter.food_static <- ingredients_static$name
class1 <- unique(ingredients_static$class)
class2 <- unique(ingredients_static$class2)

## list of ingredients that I have and don't have
ingredients.0_static <- ingredients_static %>% filter(has==0) %$% name
ingredients.1_static <- ingredients_static %>% filter(has==1) %$% name




# Define UI
ui <- tagList(
  tags$head(
    tags$style(HTML(".btn-group > .btn.active {
             background-color: steelblue;
             color: white;
             }
           
           .btn-mygrey {
             background-color: ghostwhite;
             color: grey70;
           }
             .checkboxGroupButtons .btn:hover {
  background-color: #555; /* Darker on hover */
  color: grey70;
}
          "))
  ),
  navbarPage(
  titlePanel("Recipe Tagging App"),
  tabPanel("Tag recipes",
           sidebarLayout(
             sidebarPanel(
               selectInput("recipe_select", "Select Recipe", choices = recipes_naive$Recipe),
               checkboxGroupButtons("rt_already", "Already tagged",  choices = c("Yes", "No"), selected = c("Yes", "No"),  size = "sm"),
               checkboxGroupButtons("rt_meta", "Recipe meta data",  choices = rt_meta, checkIcon = list(yes = icon("ok",  lib = "glyphicon")), size = "sm"),
               checkboxGroupButtons("rt_time", "Cooking time",  choices = rt_time, checkIcon = list(yes = icon("ok",  lib = "glyphicon")), size = "sm"),
               checkboxGroupButtons("rt_cuisine", "Cuisine",  choices = rt_cuisine, checkIcon = list(yes = icon("ok",  lib = "glyphicon")), size = "sm"),
               checkboxGroupButtons("rt_type", "Recipe type",  choices = rt_type, checkIcon = list(yes = icon("ok",  lib = "glyphicon")), size = "sm"),
               checkboxGroupButtons("rt_method", "Cooking method",  choices = rt_method, checkIcon = list(yes = icon("ok",  lib = "glyphicon")), size = "sm"),
               actionButton("add_tags", "Add Tags"),
               width = 4
             ),
             mainPanel(
               h4("Recipe Details"),
              verbatimTextOutput("ttt"),
               reactableOutput("recipe_table"),

               width = 8
             )
           )
  ),
  tabPanel("Update ingredients",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("pi_select.has", "Newly in stock", sort(ingredients.0_static), multiple = TRUE, options = NULL),
               selectizeInput("pi_select.nothas", "Fresh out", sort(ingredients.1_static), multiple = TRUE, options = NULL),
               actionButton("pi_update_makeable", "Update")
             ),
             mainPanel(
               tabsetPanel(
                 # Dynamically generate UI for each unique class
                 !!!map(unique(ingredients_static$class), function(x) {
                   tabPanel(
                     title = x,
                     div(
                       style = "height: 800px;width: 98%; overflow-y: auto;padding-left: 10px;padding-bottom: 10px; margin-bottom:10px;",  # Adjust height as needed
                       # Iterate over unique class2 values within each class
                       !!!map(unique(ingredients_static %>% filter(class == x) %>% pull(class2)), function(y) {
                         CH <- ingredients_static %>% filter(class2 == y)
                         checkboxGroupButtons(
                           inputId = paste0("i_", gsub(" ", "_", y)),
                           label = y,
                           choices = CH$name,
                           selected = CH %>% filter(has == 1) %>% pull(name), individual = FALSE, status = "mygrey", 
                           checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                           size = "sm"
                         )
                       }) # End map over class2
                     ) # End div
                   ) # End tabPanel
                 }) # End map over class
               ) # End tabsetPanel
             ) # End mainPanel
           )
  )

)
)

# Define server logic
server <- function(input, output, session) {

  # Reactive data frame to store recipes
  recipes <- reactiveValues(rdf = recipes_naive, shown=recipes_naive )

  ## List of ingredients that I have, don't have and the full ingredients df
  ingredients.rv <- reactiveValues(ingredients.0 = ingredients.0_static,
                                   ingredients.1 = ingredients.1_static,
                                   ingredients.df = ingredients_static,
                                   changed = NULL)
  
  input_ids <- paste0("i_", gsub(" ", "_", unique(ingredients_static$class2)))

  input_values <- reactive({
    
    values <- map(input_ids, function(id) {
      input[[id]]
    }) %>% unlist()
    
    values
  })
  
  
  observeEvent(input$add_tags, {

      selected_recipe <- input$recipe_select
      selected_tags <- c(input$rt_meta, input$rt_cuisine, input$rt_type, input$rt_method, input$rt_time)

      # Update the dataframe with the selected tags
      updated_recipes <- recipes$rdf

        updated_recipes[updated_recipes$Recipe == selected_recipe, "tag"] <- toString(selected_tags)

      recipes$rdf <- updated_recipes %>% mutate(tagged = ifelse(tag=="", "No", "Yes"))

  })

    observe({

      tggd <- recipes$rdf %>% filter(Recipe == input$recipe_select) %$% tag %>%
        strsplit(., split = ",") %>% unlist() %>% trimws()

      updateCheckboxGroupButtons(session, "rt_meta", selected = intersect(rt_meta, tggd))
      updateCheckboxGroupButtons(session, "rt_time", selected = intersect(rt_time, tggd))
      updateCheckboxGroupButtons(session, "rt_cuisine", selected = intersect(rt_cuisine, tggd))
      updateCheckboxGroupButtons(session, "rt_type", selected = intersect(rt_type, tggd))
      updateCheckboxGroupButtons(session, "rt_method", selected = intersect(tggd, rt_method))


    DF <- recipes$rdf %>% filter(tagged %in% input$rt_already)
    recipes$shown <- DF

    output$recipe_table <- renderReactable({
      reactable(DF,
                onClick = "select",
                defaultSelected = which(DF$Recipe==input$recipe_select),
                selection = "single", height = 700,
                searchable = TRUE, pagination = FALSE,
                columns = list(Ingredients = colDef(html = TRUE, minWidth = 275),
                               Recipe = colDef(minWidth = 150),
                               Source = colDef(minWidth = 75),
                               Page = colDef(minWidth = 40),
                               tag = colDef(minWidth = 150),
                               tagged = colDef(show = FALSE)
                ))
    })

    S <- ifelse(input$recipe_select %in% recipes$shown$Recipe,input$recipe_select, recipes$shown$Recipe[1] )

    updateSelectInput(session, "recipe_select", choices = recipes$shown$Recipe,  selected = S)

  })
    observe({

      recipe_to_tag <- req(getReactableState("recipe_table", "selected"))
      updateSelectInput(session, "recipe_select", choices = isolate(recipes$shown$Recipe),  selected = isolate(recipes$shown$Recipe[recipe_to_tag]))
    })



  output$ttt <- renderPrint({
    ingredients.rv$changed
  })


observeEvent(
  c(input$pi_update_makeable),
  {
    new.ingredients.0 <- c(setdiff(ingredients.rv$ingredients.0 ,input$pi_select.has),input$pi_select.nothas)
    new.ingredients.1 <- c(setdiff(ingredients.rv$ingredients.1 ,input$pi_select.nothas), input$pi_select.has)

    updateSelectizeInput(session, "pi_select.has", choices = new.ingredients.0, selected = NULL)
    updateSelectizeInput(session, "pi_select.nothas", choices = new.ingredients.1, selected = NULL)
    
    walk2(input_ids, gsub("_", " ", gsub("i_", "", input_ids)), 
    function(x, y) {SL <- intersect(new.ingredients.1, ingredients.rv$ingredients.df %>% filter(class2 == y) %>% pull(name))
                  updateCheckboxGroupButtons(session, x, 
                                selected = SL)
                  }
                                
    )
    
    ingredients.rv$changed <- unique(c(setdiff(new.ingredients.1, ingredients.rv$ingredients.1),
                                       setdiff(new.ingredients.0, ingredients.rv$ingredients.0),
                                       ingredients.rv$changed))
    
    # Update output df
    ingredients.rv$ingredients.1 <- new.ingredients.1
    ingredients.rv$ingredients.0 <- new.ingredients.0
    

  }  )




observe({
  new.ingredients.1 <- input_values()
  new.ingredients.0 <- setdiff(ingredients.rv$ingredients.df$name, new.ingredients.1)
  
  updateSelectizeInput(session, "pi_select.has", choices = new.ingredients.0, selected = NULL)
  updateSelectizeInput(session, "pi_select.nothas", choices = new.ingredients.1, selected = NULL)

  ingredients.rv$changed <- isolate(unique(c(setdiff(new.ingredients.1, ingredients.rv$ingredients.1),
                                             setdiff(new.ingredients.0, ingredients.rv$ingredients.0),
                                             ingredients.rv$changed))) 
  
  # Update output df
  ingredients.rv$ingredients.1 <- new.ingredients.1
  ingredients.rv$ingredients.0 <- new.ingredients.0

})


}
# Run the application
shinyApp(ui = ui, server = server)



# div(style = "display: flex; flex-wrap: wrap; gap: 25px;",  # Adjust gap for spacing between columns
#     lapply(seq(1, length(recipe_tags), by = ceiling(length(recipe_tags) / 4)), function(i) {  # Calculate chunks for 3 columns
#       div(style = "flex: 1; min-width: calc(25% - 25px);",  # 3 columns layout
#           checkboxGroupInput("Recipe tags",
#             inputId = paste0("tags_", i),
#             label = NULL,
#             choices = recipe_tags[i:min(i + ceiling(length(recipe_tags) / 4) - 1, length(recipe_tags))],
#             inline = TRUE
#           )
#       )
#     })
# )


# library(shiny)
# 
# ui <- fluidPage(
#   checkboxGroupInput("i_cheese", "Cheese", choices = c("Cheddar", "Mozzarella")),
#   checkboxGroupInput("i_dairy", "Dairy", choices = c("Milk", "Yogurt")),
#   verbatimTextOutput("input_values_print")
# )
# 
# server <- function(input, output, session) {
#   
#   input_ids <- c("i_cheese", "i_dairy")
#   
#   input_values <- reactive({
#     values <- lapply(input_ids, function(id) {
#       input[[id]]
#     })
#     names(values) <- input_ids
#     values
#   })
#   
#   output$input_values_print <- renderPrint({
#     input_values()
#   })
# }
# 
# shinyApp(ui = ui, server = server)
