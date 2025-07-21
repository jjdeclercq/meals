# WHOLE ENCHILADA
######## ADD RECIPES################
require(stringr)
require(dplyr)
require(magrittr)
require(tidyverse)
require(DT)
library(shiny)
require(ggplot2)
require(UpSetR)
require(reactable)
require(shinyWidgets)

# last part of multi-optional ingredients are not colorized

## To do
# source, page and type are preselected
# cuisine is mostly worthless

# Issues to address:
# Adding a new recipe source doesn't update in View Recipes Tab


## Ingredients to fix

## and/or/ not filters are not exclusive matches (searching 'corn' returns 'cornstarch')
# italian sausage/ breakfast sausage move to sausage - done
# cannelini beans/ sherry need fixing
# perfect easy red sauve is a duplicate recipe

source("../code/meal_funs.R")

new_expansions <- NULL
load("../data/recipes_expand.rda")


recipe_df <- read.csv("../data/recipes_out.csv")
sources <- unique(recipe_df$Source)
# cuisines <- unique(recipe_df$Cuisine)
categories <- unique(recipe_df$diet)
types <- unique(recipe_df$Type)
makea <- sort(unique(recipe_df$makeability))
pre_selected <- recipe_df %>% filter(Selected==1) %$% Recipe

# recipes_naive <- recipe_df %>% select(Meal = recipe, Source, Cuisine, Type, Ingredients = Ingredients.naive)
recipes_naive <- read.csv("../data/recipes_in.csv")%>%
  mutate(tagged = ifelse(Tags=="", "No", "Yes"))

rt_meta <- c("Favorite", "Made previously", "Kid-friendly",  "Large yield", "Good for groups","Good", "Great", "Meh")
rt_time <- c("Requires prep", "Do-ahead", "<20 minutes", "20-40 minutes", "40-80 minutes", "80-120 minutes", "2+ hours")
rt_cuisine <- c("American", "Southern", "Chinese", "Vietnamese", "Thai", "Korean", "Japanese",
                "Mexican", "Latin", "Cajun","French",  "Italian", "Mediterranian","Middle eastern","Indian")
rt_type <- c("Dressing",  "Sauce/condiment", "Ingredient", "Salad", "Soup/stew",
             "Meal","Side dish", "Main course",  "Appetizer","Breakfast", "Dessert", "Beverage")
rt_method <- c("Grilling", "Smoking", "Sous vide", "Deep fry",  "Stir fry", "Pressure cooker", "Oven/baking", "Stove top")

recipe_tags <- c(rt_meta, rt_time, rt_cuisine, rt_type, rt_method)


ingredients_static <- read.csv("../data/ingredients.csv") %>% 
  mutate(rn = 1:n()) %>% tibble::as_tibble() %>% mutate(date_changed = ymd(date_changed))
filter.food_static <- ingredients_static$name
# class1 <- unique(ingredients_static$class)
# class2 <- unique(ingredients_static$class2)

## list of ingredients that I have and don't have
ingredients.0_static <- ingredients_static %>% filter(has==0) %$% name
ingredients.1_static <- ingredients_static %>% filter(has==1) %$% name
# new.ingredients.0_static <- NULL
# new.ingredients.1_static <- NULL

def.sel <- ingredients_static %>% filter(ingredients_static$name %in% ingredients.1_static) %$% rn %>%as.numeric()

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
          "))
  ),
  navbarPage(
    "Add recipes",
    theme = shinythemes::shinytheme("spacelab"),
    ########### UI view recipes #######
    tabPanel(
      "View Recipes",
      sidebarLayout(
        sidebarPanel(
          h3("Filter Recipes"),
          checkboxGroupButtons("fr_source", "Source", sort(sources), selected = sort(sources), size = "sm", status = "mygrey"),
          selectizeInput("fr_type", "Type", sort(types), selected = sort(types), multiple = T, options = NULL),
          selectizeInput("fr_category", "Category", sort(categories), selected = sort(categories), multiple = T, options = NULL),          selectizeInput("fr_tags", "Select tags", recipe_tags, multiple = T, options = NULL),
          selectizeInput("fr_food_or", "Select ingredients (or)", sort(filter.food_static), multiple = T, options = NULL),
          selectizeInput("fr_food_and", "Select ingredients (and)", sort(filter.food_static), multiple = T, options = NULL),
          selectizeInput("fr_food_not", "Select ingredients (not)", choices = c("X", sort(filter.food_static)), multiple = TRUE, options = NULL, selected = "X"),
          sliderInput("fr_ingredient", "Ingredients Needed", min = 0, max = 10, value = c(0, 10), ticks = F),
          radioButtons("fr_make", "Makeability", makea, inline = TRUE, selected = 5),
          radioButtons("fr_selected", "Selected", c("All", "Selected"), inline = TRUE),
          checkboxGroupInput("fr_restrict", "Restrict", c("No", "Yes"), inline = TRUE, selected = "Yes"),
          actionButton("fr_submit", "Update selections")
        ),
        mainPanel(
          tabsetPanel(
          tabPanel("All", 
                   reactableOutput("filter_recipes_df")),
          tabPanel("Selected plot", 
                   plotOutput("xxx")
                   ),
          tabPanel("Selected table", 
                   
                   reactableOutput("select_test")
          )
        ))
      )
    ),
    tabPanel("Pantry",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("pi_select.has", "Newly in stock", sort(ingredients.0_static), multiple = TRUE, options = NULL),
                 selectizeInput("pi_select.nothas", "Fresh out", sort(ingredients.1_static), multiple = TRUE, options = NULL),
                 actionButton("pi_update_makeable", "Update ingredients"),
                 actionButton("pi_save", "Update recipes")
               ),
               mainPanel(
                 tabsetPanel(
                   # Dynamically generate UI for each unique class
                   !!!map(unique(ingredients_static$class), function(x) {
                     tabPanel(
                       title = x,
                       div(
                         style = "height: 800px;width: 98%; overflow-y: auto;padding-left: 10px;",  # Adjust height as needed
                         # Iterate over unique class2 values within each class
                         !!!map(unique(ingredients_static %>% filter(class == x) %>% pull(class2)), function(y) {
                           CH <- ingredients_static %>% filter(class2 == y)
                           checkboxGroupButtons(
                             inputId = paste0("i_", gsub(" ", "_", y)),
                             label = y,
                             choices = CH$name,
                             selected = CH %>% filter(has == 1) %>% pull(name), individual = FALSE, status = "mygrey", 
                             # checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                             size = "sm"
                           )
                         }) # End map over class2
                       ) # End div
                     ) # End tabPanel
                   }), # End map over class
                   tabPanel("Recent", reactableOutput("recent_ingredients"))
                 ) # End tabsetPanel
               ) # End mainPanel
             )
    ),
    # tabPanel("Pantry",
    #     
    #          sidebarLayout(
    #            sidebarPanel(
    #              selectizeInput("pi_select.has", "Newly in stock", sort(ingredients.0_static), multiple = T, options = NULL),
    #              selectizeInput("pi_select.nothas", "Fresh out", sort(ingredients.1_static), multiple = T, options = NULL),
    #              actionButton("pi_update", "Update"),
    #              actionButton("pi_react", "React"),
    #              actionButton("pi_save", "Save")
    #              
    #            ),
    #            mainPanel(
    #              tabsetPanel(
    #              tabPanel("Data", 
    #                       reactableOutput("pantry_df"),
    #                       verbatimTextOutput("selected"),
    #                       verbatimTextOutput("ttt")),
    #              tabPanel("Cloud", 
    #                       reactableOutput("pantry_cloud"))
    #              )
    #            )
    #          )
    # ),
    tabPanel(
        "Add Recipes",
        sidebarLayout(
            sidebarPanel(
                h3("Add recipe"),
                textInput("ar_name","Recipe name", NULL),
                selectizeInput("ar_source", "Recipe source",choices = sort(sources), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                numericInput("ar_page","Page number", value = NULL, width = "100px"),
                selectizeInput("ar_type", "Recipe type",choices = sort(types), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                # selectizeInput("ar_cuisine", "Cuisine",choices = sort(cuisines), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
                selectizeInput("ar_include", "Include all of:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                selectizeInput("ar_one_of", "Include one of:", c(sort(filter.food_static), "1","2","3","4","5"), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                selectizeInput("ar_optional", "Optional:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),

                actionButton("ar_submit_new_recipe", "Submit")
                # actionButton("ar_update", "Update menu")

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
          radioButtons("ai_class1", "Class:", unique(ingredients_static$class), selected = character(0), inline = TRUE),
          selectizeInput("ai_class2", "Category:", c("category"), selected = "category", multiple = FALSE),
          selectizeInput("ai_equal", "Equivalent substitution:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
          selectizeInput("ai_reach", "Occasional substitution:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
          radioButtons("ai_make", "Makeable", c("No", "Yes"), inline = TRUE),
          radioButtons("ai_has", "In stock", c("No", "Yes"), inline = TRUE, selected = "No"),
          radioButtons("ai_veg", "Dietary classification", c("Vegan", "Vegetarian", "Neither"), inline = TRUE, selected = character(0)),
          radioButtons("ai_restrict", "Restricted", c("No", "Yes"), inline = TRUE),
          actionButton("ai_submit_new_ingredient", "Submit")
       
        ),
        mainPanel(
          DT::dataTableOutput("bind_ingredients")
        )
      )
    ),
    tabPanel("Edit recipes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("recipe_select", "Select Recipe", choices = recipes_naive$Recipe),
                 checkboxGroupButtons("rt_already", "Already tagged",  choices = c("Yes", "No"), selected = c("Yes", "No"),  size = "sm", status = "mygrey"),
                 checkboxGroupButtons("rt_meta", "Recipe meta data",  choices = rt_meta,  size = "sm", status = "mygrey"),
                 checkboxGroupButtons("rt_time", "Cooking time",  choices = rt_time, size = "sm", status = "mygrey"),
                 checkboxGroupButtons("rt_cuisine", "Cuisine",  choices = rt_cuisine,  size = "sm", status = "mygrey"),
                 checkboxGroupButtons("rt_type", "Recipe type",  choices = rt_type, size = "sm", status = "mygrey"),
                 checkboxGroupButtons("rt_method", "Cooking method",  choices = rt_method, size = "sm", status = "mygrey"),
                 actionButton("add_tags", "Add Tags"),
                 h3("Edit ingredients"),
                 selectizeInput("er_include", "Include all of:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                 selectizeInput("er_one_of", "Include one of:", c(sort(filter.food_static), "1","2","3","4","5"), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                 selectizeInput("er_optional", "Optional:", sort(filter.food_static), selected = NULL, multiple = TRUE, options = list(create = FALSE)),
                 width = 4
               ),
               mainPanel(
                 h4("Recipe Details"),
                 verbatimTextOutput("ttt"),
                 reactableOutput("recipe_table"),
                 
                 width = 8
               )
             )
    )
    
    
    )

)

server <- function(input, output, session) {
  
  ####### Reactive values ########
  ## List of ingredients that I have, don't have and the full ingredients df
  ingredients.rv <- reactiveValues(ingredients.0 = ingredients.0_static,
                                   ingredients.1 = ingredients.1_static,
                                   ingredients.df = ingredients_static,
                                   changed = NULL)
 
  ## Makeable recipes
  makeable.rv <- reactiveValues(df = recipe_df,
                                RRR = recipe_df %>% filter(Recipe %in% pre_selected),
                                Selected = pre_selected)
  
  ## Complete list of ingredients
  filter.food <- reactiveValues(filter.food = filter.food_static)
  
  ## Expanded recipes
  rv.expand <- reactiveValues(rve = NULL, rv.df = recipes_naive, rv.df.shown = recipes_naive,
                              recipes_expand_new = recipes_expand)
  
  ### Proxies

  output$bind_recipes_naive <- DT::renderDataTable(
    {
      rv.expand$rv.df %>% select(-tagged, -Selected, -Cuisine, -Type) %>% 
      DT::datatable(., rownames = FALSE, escape = FALSE, style = "bootstrap")
    }, server = FALSE
  )
  

  output$bind_ingredients <- DT::renderDataTable(
    {
      DT::datatable(ingredients.rv$ingredients.df %>% select(name:date_changed), rownames = FALSE, escape = FALSE, style = "bootstrap")
    }, server = FALSE
  )
  
  observe({
  output$recent_ingredients <- renderReactable(
    {
      ingredients.rv$ingredients.df %>% select(name, has, date_changed) %>% arrange(desc(date_changed)) %>% 
        mutate("In stock" = ifelse(has == 1, name, ""), "Out of stock" = ifelse(has == 0, name, "")) %>% 
        select("In stock", "Updated on" = date_changed, "Out of stock") %>% 
      reactable(., searchable = TRUE,
                height = 800, width = 400,
                pagination = FALSE)
    }
  )})
  
  
  ########## View makeable meals #######################   
  
  observeEvent(
    c(
      input$fr_source,
      input$fr_type,
      input$fr_category,
      input$fr_tags,
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
        dplyr::select(Recipe, Source,Page, Category = diet,  Type, Needed, Ingredients,makeability, restricted, Selected, Tags) %>% 
        mutate(Selected = ifelse(Recipe %in% makeable.rv$Selected, 1, 0)) %>%
        dplyr::filter(
          Source %in% input$fr_source,
          Type %in% input$fr_type,
          Category %in% input$fr_category,
          makeability <= input$fr_make,
          restricted %in% input$fr_restrict,
          Selected >= filter_select,
          dplyr::between(Needed, input$fr_ingredient[1], input$fr_ingredient[2])) %>% 
        mutate(Source = ifelse(is.na(Page), Source, paste0(Source, " (", Page, ")")))  %>% 
        select(-Needed, -Category, -restricted)
      
      ## Filter on tags
      df <- df[grepl(andMatch(as.character(input$fr_tags)), df$Tags, perl = TRUE),]
      # df <- df[grepl(paste(as.character(input$fr_tags),collapse="|"), df$Tags),]
      
      df <- df[grepl(paste(as.character(input$fr_food_or),collapse="|"), df$Ingredients),]
      df <- df[grepl(andMatch(as.character(input$fr_food_and)), df$Ingredients, perl = TRUE),]
      df <- df[!grepl(paste(as.character(input$fr_food_not),collapse="|"), df$Ingredients),]
      
      makeable.rv$RRR <- df
      
      output$filter_recipes_df <- renderReactable(
        {
          reactable(df %>% select(-Selected), searchable = TRUE, 
                    onClick = "select", 
                    selection = "multiple",
                    height = 800, 
                    pagination = FALSE,
                    defaultSelected = which(df$Recipe %in% makeable.rv$Selected),
                    columns = list(Ingredients = colDef(html = TRUE, minWidth = 350),
                                   Recipe = colDef(minWidth = 150),
                                   Source = colDef(minWidth = 150),
                                   Page = colDef(minWidth = 50, show = FALSE), 
                                   Tags = colDef(minWidth = 200)
                                   ),
                    theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
                    resizable = TRUE)
        }
      )

    }
  )
  
  ## Make a show all button

  ## Prepare selected recipe plot
  observe({

  output$select_test <- renderReactable({
    rv.expand$recipes_expand_new %>%
      filter(Recipe %in% makeable.rv$Selected) %>%
      select(-variant) %>% group_by(Recipe) %>% summarise_all(function(x) 1*((mean(x)>0))) %>% rm.zero.cols()  %>% 
      select(Recipe, any_of(ingredients.rv$ingredients.0)) %>% 
      pivot_longer(., -Recipe) %>% 
      filter(value == 1) %>% group_by(name) %>% mutate(x = toString(Recipe), n = n()) %>% 
      select(name, x, n) %>% distinct() %>% arrange(desc(n)) %>% reactable()
  })
  output$xxx <- renderPlot(
    {
      # rv.expand$recipes_expand_new %>%
      # filter(Recipe %in% makeable.rv$Selected) %>%
      # select(-variant) %>% group_by(Recipe) %>% summarise_all(mean) %>%
      # pivot_longer(., -Recipe) %>% filter(value >0) %>%
      # group_by(name) %>%summarise(n = sum(value), count = n()) %>%
      # left_join(., ingredients.rv$ingredients.df %>% select(name, has, class), by = "name") %>% arrange(class,  desc(n)) %>%
      # mutate(name = fct_inorder(name))  %>%
      #   ggplot(., aes(x = name, y = n, fill = factor(has))) + geom_bar(stat = "identity") +
      #   facet_grid(~class, scales = "free", space = "free")+ theme_bw()+
      #   guides(fill = "none") +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x= NULL, y = NULL) +
      #   scale_fill_manual(values = c("#CD2626","#458B00"))
      

      rv.expand$recipes_expand_new %>%
         filter(Recipe %in% makeable.rv$Selected) %>%
        select(-variant) %>% group_by(Recipe) %>% summarise_all(function(x) 1*((mean(x)>0))) %>% rm.zero.cols() %>% 
        select(any_of(ingredients.rv$ingredients.0)) %>% 
        as.data.frame()%>% UpSetR::upset(nsets = 12, text.scale = 2, mb.ratio = c(0.4, 0.6), nintersects = 25)
    }, height = 600, width = 600
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
      write.csv(., "../data/recipes_out.csv", row.names = FALSE)
      
      rv.expand$rv.df%>%
        mutate(Selected = 1*(Recipe %in% makeable.rv$Selected)) %>%
      write.csv(., "../data/recipes_in.csv", row.names = FALSE)
                                    
    }, ignoreInit = TRUE)

  
  ######## Pantry ##################

  input_ids <- paste0("i_", gsub(" ", "_", unique(ingredients_static$class2)))
  
  input_values <- reactive({
    
    values <- map(input_ids, function(id) {
      input[[id]]
    }) %>% unlist()
    
    values
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
  
  ## Change pantry upon clicking checkboxes
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
  
  observeEvent(
    c(input$pi_save),
    {
      ## Step 1 - Save current selections
      save.ingredients <- bind_rows(data.frame(name = ingredients.rv$ingredients.1, has = 1),
                                    data.frame(name = ingredients.rv$ingredients.0, has = 0)) %>%
        left_join(ingredients.rv$ingredients.df %>% select(-has), ., by = "name") %>% 
        arrange(class, class2, desc(n))
      
      save.ingredients <- save.ingredients %>% mutate(date_changed = if_else(name %in% ingredients.rv$changed, ymd(Sys.Date()), date_changed ))
      ingredients.rv$ingredients.df <- save.ingredients
      
       updateReactable(outputId = "recent_ingredients" , data = ingredients.rv$ingredients.df )
      
      write.csv(save.ingredients, "../data/ingredients.csv", row.names = FALSE)
      write.csv(save.ingredients, paste0("../data/archive/ingredients_", Sys.Date(), ".csv"), row.names = FALSE)
      
      ## Step 2 - Update makeable recipes
      changed_recipes <- recipes_expand %>% select(Recipe, any_of(ingredients.rv$changed)) %>% rowwise() %>% 
        mutate(x = sum(across(any_of(ingredients.rv$changed)))) %>% filter(x > 0) %>% pull(Recipe) %>% unique()
      
      load("../data/recipes_expand.rda")
      made_menu <- make_menu2(recipes_expand %>% filter(Recipe %in% changed_recipes), save.ingredients,rv.expand$rv.df, makeable.rv$df)
      
      makeable.rv$df <- made_menu
      write.csv(made_menu, "../data/recipes_out.csv", row.names = FALSE)
      
      updateReactable(outputId = "filter_recipes_df" , data = makeable.rv$df)
      
    }, ignoreInit = TRUE)

  
  output$ttt <- renderPrint({
    
    changed_recipes <- recipes_expand %>% select(Recipe, any_of(ingredients.rv$changed)) %>% rowwise() %>% 
      mutate(x = sum(across(any_of(ingredients.rv$changed)))) %>% filter(x > 0) %>% pull(Recipe) %>% unique()
    
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
      input$ai_submit_new_ingredient,
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
                                restricted = 1*(input$ai_restrict == "Yes"),
                                date_changed = ymd((Sys.Date()))
                                )
        
        new_df.i <- new_row.i %>%
          dplyr::bind_rows(., ingredients.rv$ingredients.df )%>%
          arrange(class, class2, desc(n))
        
        ingredients.rv$ingredients.df <- new_df.i %>% 
          mutate(rn = 1:n()) %>% tibble::as_tibble() %>% select(name:date_changed, rn)
        
        write.csv(new_df.i, "../data/ingredients.csv", row.names = FALSE)
        
        # iproxy %>% addRow(new_row.i)
        
        ### Add new food to choices of add recipies
        updateSelectizeInput(session,"ar_include", choices = sort(filter.food$filter.food), selected = NULL)
        updateSelectizeInput(session,"ar_one_of", choices =c(sort(filter.food$filter.food), "1","2","3","4","5"), selected = NULL)
        updateSelectizeInput(session,"ar_optional", choices =sort(filter.food$filter.food), selected = NULL)
        
        ## Reset ingredient form
        updateTextInput(session,"ai_name","Ingredient name", value = "")
        updateRadioButtons(session,"ai_class1", "Class:", unique(ingredients_static$class), selected = character(0), inline = TRUE)
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
        input$ar_submit_new_recipe,
        {
            
            # Update data
            new_row <- data.frame(Recipe = input$ar_name,
                                  Source = input$ar_source,
                                  Page = input$ar_page,
                                  Type =  input$ar_type,
                                  Ingredients = paste.ingredients2(input$ar_optional, input$ar_one_of, input$ar_include),
                                  Selected = 0, Tags = "", tagged = "No")

            new_df <- new_row %>%
                dplyr::bind_rows(., 
                                 read.csv("../data/recipes_in.csv"))
            
            rv.expand$rv.df <- new_df
            
            write.csv(new_df, "../data/recipes_in.csv", row.names = FALSE)
            
            # Update UI
            filter.food$filter.food <- unique(c(input$ar_optional, input$ar_one_of, input$ar_include, filter.food$filter.food))

            new_sources <- sort(unique(new_df$Source))
            new_types <- sort(unique(new_df$Type))

            updateTextInput(session, "ar_name", value = "")
            updateNumericInput(session, "ar_page", value = NULL)
            updateSelectizeInput(session, "ar_source", choices = new_sources, selected = input$ar_source)
            updateSelectizeInput(session, "ar_type", choices = new_types, selected = NULL)# options = list(create = TRUE))
            updateSelectizeInput(session,"ar_include", choices = sort(filter.food$filter.food), selected = NULL)
            updateSelectizeInput(session,"ar_one_of", choices =c(sort(filter.food$filter.food), "1","2","3","4","5"), selected = NULL)
            updateSelectizeInput(session,"ar_optional", choices =sort(filter.food$filter.food), selected = NULL)
            
            ## Update tag recipes
            updateSelectInput(session, "recipe_select", choices = c(input$ar_name,rv.expand$rv.df.shown$Recipe),  selected = input$ar_name)
            
            ## HERE - change this so that any new recipe is expanded and make_menu'ed -- only the new recipe
            
            ## Expand new recipes
            expanded_new <- expand.recipes(new_row)
            rv.expand$rve <- bind_rows(rv.expand$rve,expanded_new )%>%
              mutate(across(is.numeric, ~replace_na(.x, 0)))
            
            ## All of this is from ar_update
            
            rv.expand$recipes_expand_new <- bind_rows(rv.expand$rve, rv.expand$recipes_expand_new) %>%
              mutate(across(is.numeric, ~replace_na(.x, 0)))
            
            new.ingredients <- update_ingredients(rv.expand$recipes_expand_new, ingredients.rv$ingredients.df)

            made_menu_new <- make_menu2(rv.expand$rve, new.ingredients,rv.expand$rv.df, makeable.rv$df  ) %>%
              mutate(Selected = 1*(Recipe %in% makeable.rv$Selected))
            
            recipes_expand <- rv.expand$recipes_expand_new
            makeable.rv$df <- made_menu_new
            
            updateReactable(outputId = "filter_recipes_df" , data = makeable.rv$df)
            
            write.csv(made_menu_new, "../data/recipes_out.csv", row.names = FALSE)
            write.csv(new.ingredients, "../data/ingredients.csv", row.names = FALSE)
            save(recipes_expand, file= "../data/recipes_expand.rda")

        },ignoreInit = TRUE
      
    )
 #### Add tags #######
  
  ## Add tags to recipes
  observeEvent(input$add_tags, {
    
    selected_recipe <- input$recipe_select
    selected_tags <- c(input$rt_meta, input$rt_cuisine, input$rt_type, input$rt_method, input$rt_time)
    
    # Update the dataframe with the selected tags
    updated_recipes <- rv.expand$rv.df
    
    updated_recipes[updated_recipes$Recipe == selected_recipe, "Tags"] <- toString(selected_tags)
    
    rv.expand$rv.df <- updated_recipes %>% mutate(tagged = ifelse(Tags=="", "No", "Yes"))
    
    ## Save selections to recipes_in
    write.csv(rv.expand$rv.df, "../data/recipes_in.csv", row.names = FALSE)
    
    ## update makeable recipes df
    # makeable.rv$df %<>% select(-Tags) %>% left_join(., rv.expand$rv.df %>% select(Recipe, Tags), by = "Recipe")
    makeable.rv$df[makeable.rv$df$Recipe == selected_recipe, "Tags"] <- toString(selected_tags)
    write.csv(makeable.rv$df, "../data/recipes_out.csv", row.names = FALSE)
    
    updateReactable(outputId = "filter_recipes_df" , data = makeable.rv$df)
  })
  
  
  observeEvent(c(input$rt_already,input$recipe_select, input$ar_submit_new_recipe, input$add_tags ),
               {
    
    ## Updates pre-selected Tags
    tggd <- rv.expand$rv.df %>% filter(Recipe == input$recipe_select) %$% Tags %>%
      strsplit(., split = ",") %>% unlist() %>% trimws()

    updateCheckboxGroupButtons(session, "rt_meta", selected = intersect(rt_meta, tggd))
    updateCheckboxGroupButtons(session, "rt_time", selected = intersect(rt_time, tggd))
    updateCheckboxGroupButtons(session, "rt_cuisine", selected = intersect(rt_cuisine, tggd))
    updateCheckboxGroupButtons(session, "rt_type", selected = intersect(rt_type, tggd))
    updateCheckboxGroupButtons(session, "rt_method", selected = intersect(tggd, rt_method))
    
    ## filters output DF based on tagged Y/N
    DF <- rv.expand$rv.df %>% filter(tagged %in% input$rt_already) %>% select(-Type, -Cuisine, -Selected)
    rv.expand$rv.df.shown <- DF
    

    
    ## Updates preselected recipe in the event that the recipe is not in the filtered df
    S <- ifelse(input$recipe_select %in% rv.expand$rv.df.shown$Recipe,input$recipe_select, rv.expand$rv.df.shown$Recipe[1] )
    
    updateSelectInput(session, "recipe_select", choices = rv.expand$rv.df.shown$Recipe,  selected = S)
    
  })
  
  output$recipe_table <- renderReactable({
    reactable(rv.expand$rv.df.shown,
              onClick = "select",
              defaultSelected = which(rv.expand$rv.df.shown$Recipe==input$recipe_select),
              selection = "single", height = 700,
              searchable = TRUE, pagination = FALSE,
              columns = list(Ingredients = colDef(html = TRUE, minWidth = 275),
                             Recipe = colDef(minWidth = 150),
                             Source = colDef(minWidth = 75),
                             Page = colDef(minWidth = 40),
                             Tags = colDef(minWidth = 150),
                             tagged = colDef(show = FALSE)
              ))
  })
  
  observe({
    ## Updates the selected recipe on left panel to match table selection
    recipe_to_tag <- req(getReactableState("recipe_table", "selected"))
    updateSelectInput(session, "recipe_select", choices = isolate(rv.expand$rv.df.shown$Recipe),  selected = isolate(rv.expand$rv.df.shown$Recipe[recipe_to_tag]))
  })
  
  observeEvent(c(input$recipe_select),{
    
    ING_to_edit <- rv.expand$rv.df %>% filter(Recipe == input$recipe_select) %$% Ingredients
    
    updateSelectizeInput(session,"er_include", choices = sort(filter.food$filter.food), selected = inv.include2(ING_to_edit))
    updateSelectizeInput(session,"er_one_of", choices =c(sort(filter.food$filter.food), "1","2","3","4","5"), selected = inv.exchange2(ING_to_edit))
    updateSelectizeInput(session,"er_optional", choices =sort(filter.food$filter.food), selected = inv.optional2(ING_to_edit))
    
  }
  )
    
}

shinyApp(ui = ui, server = server)
