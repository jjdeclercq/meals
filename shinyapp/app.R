# WHOLE SHEBANG
######## ADD RECIPES################
require(stringr)
require(dplyr)
require(magrittr)
require(tidyverse)
require(DT)
library(shiny)
require(dqshiny)
require(reactable)

## To do
# source, page and type are preselected
# make it so new recipe is sorted to top?
# cuisine is mostly worthless

# Issues to address:
# Adding a new recipe source doesn't update in View Recipes Tab
# Makeability is 997.7 for something...?

## Ingredients to fix
## Rice wine vinegar vs rice vinegar
## Hears of romaine vs romaine
# cornstartch - done
# green apple/ granny smith - done
# onion/ sweet onion - done
## cooking oil/ vegetable oil/ peanut oil/ etc.
## balsamic vinaigrette recipe is wrong?
## and/or/ not filters are not exclusive matches (searching 'corn' returns 'cornstarch')
# italian sausage/ breakfast sausage move to sausage

# perfect easy red sauve is a duplicate recipe

source("/Users/joshvumc/Documents/GitHub/meals/code/meal_funs.R")

new_expansions <- NULL
load("/Users/joshvumc/Documents/GitHub/meals/data/recipes_expand.rda")


recipe_df <- read.csv("/Users/joshvumc/Documents/GitHub/meals/data/recipes_out.csv")
sources <- unique(recipe_df$Source)
cuisines <- unique(recipe_df$Cuisine)
categories <- unique(recipe_df$diet)
types <- unique(recipe_df$Type)
makea <- sort(unique(recipe_df$makeability))
pre_selected <- recipe_df %>% filter(Selected==1) %$% Recipe

# recipes_naive <- recipe_df %>% select(Meal = recipe, Source, Cuisine, Type, Ingredients = Ingredients.naive)
recipes_naive <- read.csv("/Users/joshvumc/Documents/GitHub/meals/data/recipes_in.csv")


ingredients_static <- read.csv("/Users/joshvumc/Documents/GitHub/meals/data/ingredients.csv") %>% 
  mutate(rn = 1:n()) %>% tibble::as_tibble() 
filter.food_static <- ingredients_static$name
class1 <- unique(ingredients_static$class)
class2 <- unique(ingredients_static$class2)

## list of ingredients that I have and don't have
ingredients.0_static <- ingredients_static %>% filter(has==0) %$% name
ingredients.1_static <- ingredients_static %>% filter(has==1) %$% name
new.ingredients.0_static <- NULL
new.ingredients.1_static <- NULL

def.sel <- ingredients_static %>% filter(ingredients_static$name %in% ingredients.1_static) %$% rn %>%as.numeric()

ui <- navbarPage(
    "Add recipes",
    theme = shinythemes::shinytheme("paper"),
    ########### UI view recipes #######
    tabPanel(
      "View Recipes",
      sidebarLayout(
        sidebarPanel(
          h3("Filter Recipes"),
          selectizeInput("fr_source", "Source", sort(sources), selected = sort(sources), multiple = T, options = NULL),
          selectizeInput("fr_type", "Type", sort(types), selected = sort(types), multiple = T, options = NULL),
          selectizeInput("fr_category", "Category", sort(categories), selected = sort(categories), multiple = T, options = NULL),
          selectizeInput("fr_cuisine", "Cuisine", sort(cuisines), selected = sort(cuisines), multiple = T, options = NULL),
          selectizeInput("fr_food_or", "Select ingredients (or)", sort(filter.food_static), multiple = T, options = NULL),
          selectizeInput("fr_food_and", "Select ingredients (and)", sort(filter.food_static), multiple = T, options = NULL),
          selectizeInput("fr_food_not", "Select ingredients (not)", choices = c("X", sort(filter.food_static)), multiple = TRUE, options = NULL, selected = "X"),
          sliderInput("fr_ingredient", "Ingredients Needed", min = 0, max = 10, value = c(0, 10), ticks = F),
          radioButtons("fr_make", "Makeability", makea, inline = TRUE),
          radioButtons("fr_selected", "Selected", c("All", "Selected"), inline = TRUE),
          checkboxGroupInput("fr_restrict", "Restrict", c("No", "Yes"), inline = TRUE, selected = "No"),
          actionButton("fr_submit", "Update selections")
        ),
        mainPanel(
          tabsetPanel(
          tabPanel("All", 
                   reactableOutput("filter_recipes_df")),
          tabPanel("Selected", 
                   textOutput("select_test"),
                   plotOutput("xxx"))
        ))
      )
    ),
    tabPanel("Pantry",
        
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("pi_select.has", "Newly in stock", sort(ingredients.0_static), multiple = T, options = NULL),
                 selectizeInput("pi_select.nothas", "Fresh out", sort(ingredients.1_static), multiple = T, options = NULL),
                 actionButton("pi_update", "Update"),
                 actionButton("pi_react", "React"),
                 actionButton("pi_save", "Save")
                 
               ),
               mainPanel(
                 tabsetPanel(
                 tabPanel("Data", 
                          reactableOutput("pantry_df"),
                          verbatimTextOutput("selected")),
                 tabPanel("Cloud", 
                          reactableOutput("pantry_cloud"))
                 )
               )
             )
    ),
    tabPanel(
        "Add Recipes",
        sidebarLayout(
            sidebarPanel(
                h3("Add recipe"),
                textInput("ar_name","Recipe name", NULL),
                selectizeInput("ar_source", "Recipe source",choices = sort(sources), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                numericInput("ar_page","Page number", value = NULL, width = "100px"),
                selectizeInput("ar_type", "Recipe type",choices = sort(types), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                selectizeInput("ar_cuisine", "Cuisine",choices = sort(cuisines), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                selectizeInput("ar_include", "Include all of:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                selectizeInput("ar_one_of", "Include one of:", c(sort(filter.food_static), "1","2","3","4","5"), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                selectizeInput("ar_optional", "Optional:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),

                actionButton("ar_submit", "Submit"),
                actionButton("ar_update", "Update menu")

            ),
            mainPanel(
                DT::dataTableOutput("bind_recipes_naive")
            )
        )
    ),
    tabPanel(
      "Add ingredients",
      sidebarLayout(
        sidebarPanel(
          h3("Add ingredient"),
          textInput("ai_name","Ingredient name", NULL),
          radioButtons("ai_class1", "Class:", class1, selected = character(0), inline = TRUE),
          selectizeInput("ai_class2", "Category:", c("category"), selected = "category", multiple = FALSE),
          selectizeInput("ai_equal", "Equivalent substitution:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
          selectizeInput("ai_reach", "Occasional substitution:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
          radioButtons("ai_make", "Makeable", c("No", "Yes"), inline = TRUE),
          radioButtons("ai_has", "In stock", c("No", "Yes"), inline = TRUE, selected = "No"),
          radioButtons("ai_veg", "Dietary classification", c("Vegan", "Vegetarian", "Neither"), inline = TRUE, selected = character(0)),
          radioButtons("ai_restrict", "Restricted", c("No", "Yes"), inline = TRUE),
          actionButton("ai_submit", "Submit")
       
        ),
        mainPanel(
          DT::dataTableOutput("bind_ingredients")
        )
      )
    )
    )


server <- function(input, output, session) {
  
  ####### Reactive values ########
  ## List of ingredients that I have, don't have and the full ingredients df
  ingredients.rv <- reactiveValues(ingredients.0 = ingredients.0_static,
                                   ingredients.1 = ingredients.1_static,
                                   ingredients.df = ingredients_static)
  
  ## Vector of selected pantry ingredients for reactable
  react.selected.rv <- reactiveValues(vec=ingredients_static %>% filter(name %in% ingredients.1_static) %$% rn)
 
  ## Makeable recipes
  makeable.rv <- reactiveValues(df = recipe_df,
                                RRR = recipe_df %>% filter(Recipe %in% pre_selected),
                                Selected = pre_selected)
  
  ## Complete list of ingredients
  filter.food <- reactiveValues(filter.food = filter.food_static)
  
  ## Expanded recipes
  rv.expand <- reactiveValues(rve = NULL, rv.df = recipes_naive,
                              recipes_expand_new = recipes_expand)
  
  ### Proxies
  # bind_recipes_naive <- read.csv("/Users/joshvumc/Dropbox/meal_planning/DATA/Meals - Sheet1.csv")
  output$bind_recipes_naive <- DT::renderDataTable(
    {
      DT::datatable(recipes_naive, rownames = FALSE, escape = FALSE, style = "bootstrap")
    }, server = FALSE
  )
  
  # bind_ingredients <- read.csv("/Users/joshvumc/Dropbox/meal_planning/DATA/ingredients.csv") %>% 
  #   tibble::as_tibble() 
  output$bind_ingredients <- DT::renderDataTable(
    {
      DT::datatable(ingredients.rv$ingredients.df %>% select(name:restricted), rownames = FALSE, escape = FALSE, style = "bootstrap")
    }, server = FALSE
  )
  
  myproxy = DT::dataTableProxy(outputId = "bind_recipes_naive")
  iproxy = DT::dataTableProxy(outputId = "bind_ingredients")
  
  ########## View makeable meals #######################   
  
  observeEvent(
    c(
      input$fr_source,
      input$fr_type,
      input$fr_category,
      input$fr_cuisine,
      input$fr_ingredient,
      input$fr_food_or,
      input$fr_food_and,
      input$fr_food_not,
      input$fr_make,
      input$fr_selected,
      input$fr_submit,
      input$fr_restrict
    ),

    {
      filter_select <- ifelse(input$fr_selected == "All", 0, 1)
      df <- makeable.rv$df
      df %<>% 
        dplyr::select(Recipe, Source,Page, Category = diet, Cuisine, Type, Needed, Ingredients,makeability, restricted, Selected) %>% 
        mutate(Selected = ifelse(Recipe %in% makeable.rv$Selected, 1, 0)) %>%
        dplyr::filter(
          Source %in% input$fr_source,
          Type %in% input$fr_type,
          Category %in% input$fr_category,
          Cuisine %in% input$fr_cuisine,
          makeability <= input$fr_make,
          restricted %in% input$fr_restrict,
          Selected >= filter_select,
          dplyr::between(Needed, input$fr_ingredient[1], input$fr_ingredient[2])) %>% select(-Needed, -Category, -restricted)
      
      df <- df[grepl(paste(as.character(input$fr_food_or),collapse="|"), df$Ingredients),]
      df <- df[grepl(andMatch(as.character(input$fr_food_and)), df$Ingredients, perl = TRUE),]
      df <- df[!grepl(paste(as.character(input$fr_food_not),collapse="|"), df$Ingredients),]
      
      makeable.rv$RRR <- df
      
      output$filter_recipes_df <- renderReactable(
        {

          reactable(df %>% select(-Selected), searchable = TRUE, 
                    onClick = "select", 
                    selection = "multiple",
                    defaultSelected = which(df$Recipe %in% makeable.rv$Selected),
                    columns = list(Ingredients = colDef(html = TRUE, minWidth = 300),
                                   Recipe = colDef(minWidth = 150),
                                   Source = colDef(minWidth = 150),
                                   Page = colDef(minWidth = 50)),
                    theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
                    resizable = TRUE)
          
        }
      )

    }
  )
  
  ## Make a show all button

  ## Prepare selected recipe plot
  observe({

  output$select_test <- renderText(makeable.rv$Selected)
  output$xxx <- renderPlot(
    {
      rv.expand$recipes_expand_new %>%
      filter(Recipe %in% makeable.rv$Selected) %>%
      select(-variant) %>% group_by(Recipe) %>% summarise_all(mean) %>%
      pivot_longer(., -Recipe) %>% filter(value >0) %>%
      group_by(name) %>%summarise(n = sum(value), count = n()) %>%
      left_join(., ingredients.rv$ingredients.df %>% select(name, has, class), by = "name") %>% arrange(class,  desc(n)) %>%
      mutate(name = fct_inorder(name))  %>%
        ggplot(., aes(x = name, y = n, fill = factor(has))) + geom_bar(stat = "identity") +
        facet_grid(~class, scales = "free", space = "free")+ theme_bw()+
        guides(fill = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x= NULL, y = NULL) +
        scale_fill_manual(values = c("#CD2626","#458B00"))
    }
  )
  } )

  # Select meals
  observeEvent(
    c(input$fr_submit),
    {
     X <- getReactableState("filter_recipes_df", "selected")
     Y <- setdiff(1:nrow(makeable.rv$RRR), X)
     not.selected <- unique(c(makeable.rv$RRR$Recipe[Y]))
     current.select <- unique(c(makeable.rv$RRR$Recipe[X]))
     prev.select <- makeable.rv$Selected
      
      makeable.rv$Selected <- unique(setdiff(c(prev.select, current.select),not.selected))
      
      
      makeable.rv$df %>%
        mutate(Selected = 1*(Recipe %in% makeable.rv$Selected)) %>%
      write.csv(., "/Users/joshvumc/Documents/GitHub/meals/data/recipes_out.csv", row.names = FALSE)
      
      rv.expand$rv.df%>%
        mutate(Selected = 1*(Recipe %in% makeable.rv$Selected)) %>%
      write.csv(., "/Users/joshvumc/Documents/GitHub/meals/data/recipes_in.csv", row.names = FALSE)
                                    
    }, ignoreInit = TRUE)

  
  ######## Pantry ##################

  observeEvent(
    c(input$pi_react),
    {
      react.selected.rv$vec <- getReactableState("pantry_df", "selected")
    })
  output$pantry_df <- renderReactable({
    
    # bind_rows(data.frame(name = ingredients.rv$ingredients.1, has = 1),
    #           data.frame(name = ingredients.rv$ingredients.0, has = 0)) %>%
    #   left_join(ingredients.rv$ingredients.df %>% select(-has), ., by = "name") %>%
      
    ingredients.rv$ingredients.df %<>% 
      mutate(has = ifelse(name %in% ingredients.rv$ingredients.1, 1, 0))%>%
      arrange(class, class2, desc(n) )
    
    ingredients.rv$ingredients.df  %>%
      select(name, has, class, class2, n)%>%
      reactable(.,
                minRows = 10, 
                groupBy = c("class", "class2"), 
                highlight = TRUE, 
                compact = TRUE, 
                onClick = "select", 
                selection = "multiple",
                theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
                defaultSelected = ingredients.rv$ingredients.df %>% filter(name %in% ingredients.rv$ingredients.1) %$% rn %>%as.numeric(),
                searchable = TRUE,
                filterable = TRUE,
                fullWidth = FALSE,
                columns = list(
                  has = colDef(minWidth = 75),   # 50% width, 200px minimum
                  n = colDef(minWidth = 75),   # 25% width, 100px minimum
                  name = colDef(minWidth = 250),
                  class = colDef(minWidth = 150),
                  class2 = colDef(minWidth = 150)
                  # 25% width, 100px minimum
                ),#,
                
      )
  })
  
  observeEvent(
    c(input$pi_update),
    {
      
      new.ingredients.0 <- c(setdiff(ingredients.rv$ingredients.0 ,input$pi_select.has),input$pi_select.nothas)
      new.ingredients.1 <- c(setdiff(ingredients.rv$ingredients.1 ,input$pi_select.nothas), input$pi_select.has)
      
      updateSelectizeInput(session, "pi_select.has", choices = new.ingredients.0, selected = NULL)
      updateSelectizeInput(session, "pi_select.nothas", choices = new.ingredients.1, selected = NULL)
      
      # Update output df
      ingredients.rv$ingredients.1 <- new.ingredients.1
      ingredients.rv$ingredients.0 <- new.ingredients.0
      
      
    }    )
  
  observeEvent(
    c(react.selected.rv$vec),
    {
      new.ingredients.1b <- unique(ingredients.rv$ingredients.df[react.selected.rv$vec, "name"]$name, ingredients.rv$ingredients.1)
      new.ingredients.0b <- setdiff(ingredients.rv$ingredients.df$name, new.ingredients.1b) ### This will need updating when combining
      
      updateSelectizeInput(session, "pi_select.has", choices = new.ingredients.0b, selected = NULL)
      updateSelectizeInput(session, "pi_select.nothas", choices = new.ingredients.1b, selected = NULL)
      
      ingredients.rv$ingredients.1 <- new.ingredients.1b
      ingredients.rv$ingredients.0 <- new.ingredients.0b
      
    })
  
  observeEvent(
    c(input$pi_save),
    {
      ## Step 1 - Save current selections
      save.ingredients <- bind_rows(data.frame(name = ingredients.rv$ingredients.1, has = 1),
                                    data.frame(name = ingredients.rv$ingredients.0, has = 0)) %>%
        left_join(ingredients.rv$ingredients.df %>% select(-has), ., by = "name") %>% 
        # select(names(ingredients.rv$ingredients.df), -rn)%>%
        arrange(class, class2, desc(n))
      
      write.csv(save.ingredients, "/Users/joshvumc/Documents/GitHub/meals/data/ingredients.csv", row.names = FALSE)
      write.csv(save.ingredients, paste0("/Users/joshvumc/Documents/GitHub/meals/data/archive/ingredients_", Sys.time(), ".csv"), row.names = FALSE)
      
      ## Step 2 - Update makeable recipes
      load("/Users/joshvumc/Documents/GitHub/meals/data/recipes_expand.rda")
      made_menu <- make_menu(recipes_expand, save.ingredients, rv.expand$rv.df)
      
      makeable.rv$df <- made_menu
      write.csv(made_menu, "/Users/joshvumc/Documents/GitHub/meals/data/recipes_out.csv", row.names = FALSE)
      
    }, ignoreInit = TRUE)

  ## Pantry cloud ##
  output$pantry_cloud <- renderReactable({
    
    ingredients.rv$ingredients.df %>% 
      group_by(class, class2) %>% 
      summarise(Ingredients = toString(name))%>%
      mutate(Ingredients = sprintj6(all_of(ingredients.rv$ingredients.1), Ingredients, "#458B00"))%>%
      mutate(Ingredients = sprintj6(all_of(ingredients.rv$ingredients.0), Ingredients, "#CD2626")) %>%
      mutate(Ingredients = gsub("xyz", " or ", gsub("><",">, <",gsub("&","", Ingredients)))) %>%
      reactable(., columns = list(Ingredients = colDef(html = TRUE, width = 660),
                                  class = colDef(minWidth = 100),
                                  class2 = colDef(minWidth = 100)),
                groupBy = c("class"), 
                highlight = TRUE, 
                theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
                searchable = TRUE,
                filterable = TRUE)
    
  })
  
  ######### add recipes and ingredients ########
  

    
    # This prevents the user from inputting a duplicate name,
    ## need to figure out how/ where to put the error message
     # observeEvent(input$ar_name,
     #              validate(need(input$ar_name %nin% meals, "Select unique recipe name"))
     # )
     # 
    observeEvent(
      input$ai_class1,
      {
        ## Dynamically update the class2 selections
        updateSelectizeInput(session, "ai_class2", choices = ingredients.rv$ingredients.df %>% filter(class == input$ai_class1) %$% class2 %>% unique(), 
                             selected = NULL)
      }
    )
    ####### Update ingredients list #####
    observeEvent(    
      input$ai_submit,
      {
        filter.food$filter.food <- c( filter.food$filter.food, input$ai_name)
          
        new_row.i <- data.frame(name= input$ai_name,
                                n = 0, 
                                has = 1*(input$ai_has == "Yes"), 
                                class = input$ai_class1,
                                class2 = input$ai_class2,
                                vegetarian = 1*(input$ai_veg %in% c("Vegan", "Vegetarian")),
                                vegan = 1*(input$ai_veg %in% c("Vegan")),
                                equal = toString(input$ai_equal),
                                sometimes = toString(input$ai_reach),
                                makeable = 1*(input$ai_make == "Yes"),
                                restricted = 1*(input$ai_restrict == "Yes")
                                )
        
        new_df.i <- new_row.i %>%
          dplyr::bind_rows(., ingredients.rv$ingredients.df )%>%
          arrange(class, class2, desc(n))
        
        ingredients.rv$ingredients.df <- new_df.i %>% 
          mutate(rn = 1:n()) %>% tibble::as_tibble() %>% select(name:restricted, rn)
        
        write.csv(new_df.i, "/Users/joshvumc/Documents/GitHub/meals/data/ingredients.csv", row.names = FALSE)
        
        iproxy %>% addRow(new_row.i)
        
        ### Add new food to choices of add recipies
        updateSelectizeInput(session,"ar_include", choices = sort(filter.food$filter.food), selected = NULL)
        updateSelectizeInput(session,"ar_one_of", choices =c(sort(filter.food$filter.food), "1","2","3","4","5"), selected = NULL)
        updateSelectizeInput(session,"ar_optional", choices =sort(filter.food$filter.food), selected = NULL)
        
        
        ## Reset ingredient form
        updateTextInput(session,"ai_name","Ingredient name", value = "")
        updateRadioButtons(session,"ai_class1", "Class:", class1, selected = character(0), inline = TRUE)
        updateSelectizeInput(session,"ai_class2", "Category:", c("category"), selected = "category")
        updateSelectizeInput(session,"ai_equal", "Equivalent substitution:", sort(filter.food$filter.food), selected = NULL)
        updateSelectizeInput(session,"ai_reach", "Occasional substitution:", sort(filter.food$filter.food), selected = NULL)
        updateRadioButtons(session,"ai_make", "Makeable", c("No", "Yes"), inline = TRUE)
        updateRadioButtons(session,"ai_restrict", "Restricted", c("No", "Yes"), inline = TRUE)
        updateRadioButtons(session,"ai_has", "In stock", c("No", "Yes"), inline = TRUE, selected = "No")
        updateRadioButtons(session,"ai_veg", "Dietary classification", c("Vegan", "Vegetarian", "Neither"), inline = TRUE, selected = character(0))
        
        ## Update pantry ingredients (if in stock)
        ingredients.rv$ingredients.1 <-if(input$ai_has == "Yes"){c(ingredients.rv$ingredients.1, input$ai_name)}
        ingredients.rv$ingredients.0 <-if(input$ai_has != "Yes"){c(ingredients.rv$ingredients.0, input$ai_name)}
        
        updateSelectizeInput(session, "pi_select.nothas", choices = ingredients.rv$ingredients.1, selected = NULL)
        updateSelectizeInput(session, "pi_select.has", choices = ingredients.rv$ingredients.0, selected = NULL)
      }
      
    )
    ######### Add new recipes####
    observeEvent(    
        input$ar_submit,
        {
            
            # Update data
            new_row <- data.frame(Recipe = input$ar_name,
                                  Source = input$ar_source,
                                  Page = input$ar_page,
                                  Cuisine = input$ar_cuisine,
                                  Type =  input$ar_type,
                                  Ingredients = paste.ingredients2(input$ar_optional, input$ar_one_of, input$ar_include))

            new_df <- new_row %>%
                dplyr::bind_rows(., 
                                 read.csv("/Users/joshvumc/Documents/GitHub/meals/data/recipes_in.csv"))
            
            rv.expand$rv.df <- new_df
            
            write.csv(new_df, "/Users/joshvumc/Documents/GitHub/meals/data/recipes_in.csv", row.names = FALSE)
            
            # Update UI
            filter.food$filter.food <- unique(c(input$ar_optional, input$ar_one_of, input$ar_include, filter.food$filter.food))

            new_sources <- sort(unique(new_df$Source))
            new_cuisines <- sort(unique(new_df$Cuisine))
            new_types <- sort(unique(new_df$Type))

            updateTextInput(session, "ar_name", value = "")
            updateNumericInput(session, "ar_page", value = NULL)
            updateSelectizeInput(session, "ar_source", choices = new_sources, selected = input$ar_source)
            updateSelectizeInput(session, "ar_cuisine", choices = new_cuisines, selected = NULL)
            updateSelectizeInput(session, "ar_type", choices = new_types, selected = NULL)# options = list(create = TRUE))
            updateSelectizeInput(session,"ar_include", choices = sort(filter.food$filter.food), selected = NULL)
            updateSelectizeInput(session,"ar_one_of", choices =c(sort(filter.food$filter.food), "1","2","3","4","5"), selected = NULL)
            updateSelectizeInput(session,"ar_optional", choices =sort(filter.food$filter.food), selected = NULL)

            myproxy %>% addRow(new_row)
            
            ## Expand new recipes
            expanded_new <- expand.recipes(new_row)
            rv.expand$rve <- bind_rows(rv.expand$rve,expanded_new )%>%
              mutate(across(is.numeric, ~replace_na(.x, 0)))

        },ignoreInit = TRUE
      
    )
    
    ### Update makeable recipes and tally of ingredients used
    observeEvent(
      input$ar_update,
      {
  
        rv.expand$recipes_expand_new <- bind_rows(rv.expand$rve, rv.expand$recipes_expand_new) %>%
          mutate(across(is.numeric, ~replace_na(.x, 0)))
        
        new.ingredients <- update_ingredients(rv.expand$recipes_expand_new, ingredients.rv$ingredients.df)
        ## HERE, can I just make_menu on the new recipe?
        made_menu_new <- make_menu(rv.expand$recipes_expand_new, new.ingredients, rv.expand$rv.df ) %>%
          mutate(Selected = 1*(Recipe %in% makeable.rv$Selected))
        
        recipes_expand <- rv.expand$recipes_expand_new
        makeable.rv$df <- made_menu_new
        
        write.csv(made_menu_new, "/Users/joshvumc/Documents/GitHub/meals/data/recipes_out.csv", row.names = FALSE)
        write.csv(new.ingredients, "/Users/joshvumc/Documents/GitHub/meals/data/ingredients.csv", row.names = FALSE)
        save(recipes_expand, file= "/Users/joshvumc/Documents/GitHub/meals/data/recipes_expand.rda")
      }
    )

    
}

shinyApp(ui = ui, server = server)


# output$selected_recipes_df <-   renderReactable(
#   {
#     df <- makeable.rv$df
#     df %<>% dplyr::filter(Selected ==1) %>%
#       dplyr::select(Selected, Recipe, Source,Page,  Cuisine, Type, Ingredients,makeability)
#     SL <- which(df$Recipe %in% makeable.rv$Selected)
#     reactable(df %>% select(-Selected), searchable = TRUE, 
#               onClick = "select", 
#               selection = "multiple",
#               defaultSelected = SL,
#               columns = list(Ingredients = colDef(html = TRUE, minWidth = 300),
#                              Recipe = colDef(minWidth = 150),
#                              Source = colDef(minWidth = 150),
#                              Page = colDef(minWidth = 50)),
#               theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
#               resizable = TRUE)
#   }
# )
# 
