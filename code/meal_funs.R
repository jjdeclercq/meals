## easy yes-no of binary variables
yesno <- function(logic) {ifelse(logic, "Yes", "No")}

expand.recipes <- function(m){
  m %<>% separate(., Ingredients, c("Ingredients", "Optional"), sep= "\\[") %>% ## Remove optional ingredients from the expansion
    mutate(Ingredients = gsub("[()]|\\[|\\]", "", Ingredients))
  ing <- str_split(m$Ingredients, ", ") 
  names(ing) <- m$Recipe
  
  bind_rows(lapply(as.list(1:length(ing)), FUN = function(x) mf(x, ing = ing))) %>%
    mutate(across(where(is.numeric), .fns = ~replace_na(.x, 0)))
  
}

mf <- function(x, ing) {
  expand.grid(c(str_split(grep(" or ", ing[[x]], v = TRUE, inv = F), " or "),
                item = list(grep(" or ", ing[[x]], v = TRUE, inv = TRUE))))%>% 
    mutate(v =1, Recipe = names(ing)[x], VarXX = " ")%>%  unite(.,"variant", contains("Var"), remove = FALSE, sep = " ") %>% select(-VarXX)%>%
    pivot_longer(., c(-v, -Recipe,-variant)) %>% unique() %>% select(-name) %>% mutate(value = str_trim(value, "both"))%>%
    filter(value != "") %>% 
    pivot_wider(., names_from = "value", values_from = "v")}


nameify <- function(var){
  eq <- rlang::ensym(var)
  ifelse(var==0, "_", deparse(substitute(eq)))
}

nameify2 <- function(var){
  eq <- rlang::ensym(var)
  ifelse(var==0, "", deparse(substitute(eq)))
}

sprintj6 <- function(vec, V, color){
  V <- gsub(" or | or$", "&xyz&", gsub(" or&", "&xyz& &", gsub("& | &","&", paste0("&",gsub(",","&&",V), "&"))))
  len <- length(vec)
  for(i in 1:length(vec)){
    V <- gsub(paste0("&",vec[i],"&"), sprintf("<span style='color: %s;'>%s</span>", color, vec[i]), V)
  }
  V
}

sort.vec <- function(vec) vec[order(nchar(vec), vec, decreasing = TRUE)]


f.optional <- function(vec) {ifelse(length(vec)==0, "", paste0(str_c(vec, collapse = " or "), " or , "))}
f.exchange <- function(vec) {ifelse(length(vec)==0, "", gsub(" or [0-9]+ or", ",", paste0(str_c(vec, collapse = " or "), ", ")))}
f.include <- function(vec) {str_c(vec, collapse = ", ")}
paste.ingredients <- function(opt, exc, inc) paste0(f.optional(opt),f.exchange(exc), f.include(inc), collapse = "")

andMatch <- function(string){
  string <- c(" ", string)
  gsub(" )", "^", paste(paste(string,collapse = ")(?=.*"), ")", sep = ""))
}

regex.ingredients <- function(dat){
  dat %>% mutate(Ingredients = gsub(" or ", "&xyz&",  gsub("& | &|&  ","&", paste0("&",gsub(",","&,&",Ingredients), "&"))) )%>%
    mutate(Ingredients = gsub("&(", "&(&", Ingredients, fixed = TRUE)) %>%
    mutate(Ingredients = gsub(")&", "&)&", Ingredients, fixed = TRUE)) %>%
    mutate(Ingredients = gsub("&[", "&[&", Ingredients, fixed = TRUE)) %>%
    mutate(Ingredients = gsub("]&", "&]&", Ingredients, fixed = TRUE))
}

sprintj7 <- function(vec, V, color){
  # V <- gsub(" or | or$", "&xyz&", gsub(" or&", "&xyz& &", gsub("& | &","&", paste0("&",gsub(",","&&",V), "&"))))
  len <- length(vec)
  for(i in 1:length(vec)){
    V <- gsub(paste0("&",vec[i],"&"), sprintf("<span style='color: %s;'>%s</span>", color, vec[i]), V)
  }
  V
}


make_menu <- function(DF, ING, MEALS){
  
  ## vectors of ingredients both in and out of stock
  has <- ING %>% filter(has == 1) %$% name
  not.has <- ING %>% filter(has == 0) %$% name 
  not.has <- not.has[not.has %in% ING$name] ## not sure what this is doing -- glossing over entry errors?
  
  ## vectors of dietary restrictions
  vegan <- ING %>% filter(vegan==1) %$% name 
  veg <- ING %>% filter(vegetarian==1) %$% name
  not.restricted <- ING %>% filter(restricted == 0) %$% name
  
  # vector of ingredients that are in stock that are equivalent substitutions for other ingredients
  equi <- ING %>% select(name, equal) %>%
    separate_rows(., equal, sep = ", ") %>%
    left_join(., ING %>% select(equal = name, has2 = has), by = "equal") %>%
    filter(has2==1) %$% name %>% unique()
  equi <- c(has, equi)%>% unique()

  ## vector of ingredients that are in stock that can sometimes be substituted for other ingredients
  sometimes <- ING %>% select(name, sometimes) %>%
    separate_rows(., sometimes, sep = ", ") %>%
    left_join(., ING %>% select(sometimes = name, has2 = has), by = "sometimes") %>%
    filter(has2==1) %$% name %>% unique()
  sometimes <- c(sometimes, equi) %>% unique()
  
  ### Things I don't have, but I can substitute for
  subs1 <- ING %>% select(name,has, equal) %>%
    separate_rows(., equal, sep = ", ") %>%
    left_join(., ING %>% select(equal = name, has2 = has), by = "equal") %>% 
    filter(has2==1, has==0) %$% name %>% unique()
  
  ## Things not in stock that there can sometimes be substituted for
  subs2 <- ING %>% select(name,has, sometimes) %>%
    separate_rows(., sometimes, sep = ", ") %>%
    left_join(., ING %>% select(sometimes = name, has2 = has), by = "sometimes") %>% 
    filter(has2==1, has==0) %$% name %>% unique() %>% setdiff(., subs1)
  
  ## List of ingredients that are not in stock or cannot be substitued for
  nh <- setdiff(not.has, c(subs1, subs2))
  
  ## Sort ingredient vectors by length
  has <- sort.vec(has)
  nh <- sort.vec(nh)
  subs1 <- sort.vec(subs1)
  subs2 <- sort.vec(subs2)
  
  
  MEAL <- DF %>% 
    rowwise() %>%
    mutate(n.ingredients = sum(c_across(where(is.numeric))), ## Number of ingredients in recipe variant
           p.ingredients = sum(c_across(any_of(has))), ## Number of ingredients in stock for recipe variant
           can.make = 1*(p.ingredients == n.ingredients), ## If n and p match, then the recipe is makeable
           e.ingredients = sum(c_across(any_of(c(has, subs1)))), ## Number of ingredients or equivalent substitutions in stock for recipe variant
           can.make2 = max(e.ingredients == n.ingredients, can.make), ## If n and e match, then the recipe is makeable
           s.ingredients = sum(c_across(any_of(c(has, subs1, subs2)))), ## Number of ingredients or sometimes substitutions in stock for recipe variant
           can.make3 = max(s.ingredients == n.ingredients, can.make2), ## If n and s match, then the recipe is makeable
           n.vegan = sum(c_across(any_of(vegan))), ## Number of vegan ingredients
           is.vegan = n.vegan == n.ingredients, ## Determine if recipe is vegan
           n.veg = sum(c_across(any_of(veg))), ## Number of vegetarian ingredients
           is.veg = n.veg == n.ingredients, ## Determine if recipe is vegetarian
           n.restrict = sum(c_across(any_of(not.restricted))), ## Number of not restricted ingredients
           is.restrict = n.restrict == n.ingredients ## Determine if recipe is not restricted
    ) %>% 
    group_by(Recipe, variant)%>% # determine makability
    mutate(makeability = 5 - (1+sum(c_across(contains("can.make")))), 
           makeability = ifelse(makeability==4, 5, makeability))%>% 
    mutate(makeability = replace_na(makeability, 995))
  
  ## Makeability
  # 1 = makeable with no substitutions
  # 2 = makeabile with equivalent ingredients
  # 3 = makeabile with substitutable ingredients
  # 4 = makeable after making intermediate ingredients
  # 5 = not makeable
  
  ## some ingredients are themselves recipes
  ## This subsets those ingredients and cross references it with the makability rating 
  ## make4 is the list of ingredients not in stock but can be made in one shape or form
  make4 <- ING %>% filter(makeable ==1) %>% 
    select(name, has) %>%
    left_join(., 
              MEAL%>%ungroup() %>% select(name = Recipe, makeability) %>% 
                group_by(name) %>% summarise(makeability = min(makeability, na.rm = TRUE)),
              by = "name") %>% 
    filter(makeability %in% 1:3, has == 0) %$% name
  
  ## 'reaching' is the vector of ingredient that are in stock + equivalent + substitutible or makeable ingredients 
  reaching <- c(has, equi, sometimes, make4) %>% unique()
  
  ## limit make4 to the lowest level of availability
  make4 <- setdiff(make4, c(subs1, subs2))
  
  ## update vector of ingredients that are not available
  nh <- setdiff(nh, make4)
  
  ## fill in makeability 4 designation
  ## Determine if any of the recipe variants meet dietary restrictions AND equal to the best makeability rating
  ## Arrange data and pick the most makeabile variant with the most ingredients
  ## Add back the orginal ingredients list
  MEAL.slice <- MEAL %>% 
    mutate(m.ingredients = sum(c_across(any_of(reaching))), 
           can.make4 = 1*(m.ingredients == n.ingredients),
           makeability = ifelse(makeability==5 & can.make4==1, 4, makeability)) %>%
    select(1, contains("make"), everything()) %>% 
    arrange(Recipe, makeability, desc(n.ingredients))%>%
    mutate(makeable.restrict = is.restrict == TRUE & makeability == min(makeability),
           makeable.vegan = is.vegan == TRUE & makeability == min(makeability),
           makeable.veg = is.veg == TRUE & makeability == min(makeability)) %>%
    select(Recipe, variant,makeability, n.ingredients, contains("make"), n.vegan, is.vegan, n.veg, is.veg, everything())%>% 
    group_by(Recipe) %>%
    dplyr::slice(1) %>%
    mutate(diet = case_when(is.vegan ~ "Vegan",
                            is.veg & !is.vegan ~ "Vegetarian",
                            TRUE ~ "Non-vegetarian"),
           restricted = yesno(makeable.restrict),
           Needed = n.ingredients - p.ingredients) %>%
    left_join(MEALS , ., by = "Recipe") 
  
  ## Colorize ingredients based on what level of 'in stock' they are
  MEAL.slice %>%
    select(Recipe,  Source, Page,Type, Cuisine,diet,restricted, Ingredients, makeability, Needed, Selected) %>%
    # separate_rows(., "Ingredients", sep = ",") %>% 
    # mutate(X = case_when(grepl(" or $", Ingredients) ~ "Optional",
    #                      grepl(" or ", Ingredients) ~ "One of",
    #                      TRUE ~ "Essential")) %>%
    # mutate(Ingredients =   case_when(X == "Essential" ~ Ingredients,
    #                                  X == "Optional" ~ paste0("[", str_trim(gsub(" or ", ", ", gsub(" or $", "", Ingredients))), "]"),
    #                                  X== "One of" ~ paste0( "(", str_trim(Ingredients), ")"))) %>%
    # group_by(recipe, Source, Page, Cuisine, diet, Type, restricted, makeability, Needed) %>% arrange(X) %>% 
    # summarise(Ingredients = toString(Ingredients), .groups = "drop")%>%
    regex.ingredients() %>%
    mutate(Ingredients = sprintj8(all_of(has), Ingredients, "#458B00"))%>%
    mutate(Ingredients = sprintj8(all_of(nh), Ingredients, "#CD2626"))%>%
    mutate(Ingredients = sprintj8(all_of(subs1), Ingredients, "#CDAD00"))%>%
    mutate(Ingredients = sprintj8(all_of(subs2), Ingredients, "orange"))%>%
    mutate(Ingredients = sprintj8(make4, Ingredients, "#1C86EE"))%>%
    mutate(Ingredients = gsub("xyz", " or ", gsub("><",">, <",gsub(",", ", ", gsub("&","", Ingredients))))) %>%
    mutate(Selected = replace_na(Selected, 0))
  
}  

sprintj8 <- function(vec, V, color){
  if(length(vec)==0){ 
    V <- V
    }else{
  for(i in 1:length(vec)){
    V <- gsub(paste0("&",vec[i],"&"), sprintf("<span style='color: %s;'>%s</span>", color, vec[i]), V)
  }
      
  }
  return(V)
}

# make_menu <- function(DF, ING, MEALS){
#   
#   ## vectors of ingredients both in and out of stock
#   has <- ING %>% filter(has == 1) %$% name
#   not.has <- ING %>% filter(has == 0) %$% name 
#   not.has <- not.has[not.has %in% ING$name] ## not sure what this is doing -- glossing over entry errors?
#   
#   ## vectors of dietary restrictions
#   vegan <- ING %>% filter(vegan==1) %$% name 
#   veg <- ING %>% filter(vegetarian==1) %$% name
#   
#   equi <- ING %>% select(name, equal) %>%
#     separate_rows(., equal, sep = ", ") %>%
#     left_join(., ING %>% select(equal = name, has2 = has), by = "equal") %>% 
#     filter(has2==1) %$% name %>% unique()
#   equi <- c(has, equi)%>% unique()
#   
#   sometimes <- ING %>% select(name, sometimes) %>%
#     separate_rows(., sometimes, sep = ", ") %>%
#     left_join(., ING %>% select(sometimes = name, has2 = has), by = "sometimes") %>% 
#     filter(has2==1) %$% name %>% unique()
#   sometimes <- c(sometimes, equi) %>% unique()
#   
#   ### Things I don't have, but I can substitute for
#   subs1 <- ING %>% select(name,has, equal) %>%
#     separate_rows(., equal, sep = ", ") %>%
#     left_join(., ING %>% select(equal = name, has2 = has), by = "equal") %>% 
#     filter(has2==1, has==0) %$% name %>% unique()
#   
#   subs2 <- ING %>% select(name,has, sometimes) %>%
#     separate_rows(., sometimes, sep = ", ") %>%
#     left_join(., ING %>% select(sometimes = name, has2 = has), by = "sometimes") %>% 
#     filter(has2==1, has==0) %$% name %>% unique() %>% setdiff(., subs1)
#   
#   nh <- setdiff(not.has, c(subs1, subs2))
#   
#   
#   has <- sort.vec(has)
#   nh <- sort.vec(nh)
#   subs1 <- sort.vec(subs1)
#   subs2 <- sort.vec(subs2)
#   
#   
#   
#   MEAL <- DF %>% 
#     rowwise() %>%
#     mutate(n.ingredients = sum(c_across(where(is.numeric))))  %>% 
#     mutate(p.ingredients = sum(c_across(any_of(has))), can.make = 1*(p.ingredients == n.ingredients),
#            sub1 = sum(c_across(any_of(equi))), can.make2 = max(sub1 == n.ingredients, can.make), ## This can probably be reworked with the new subs1 variable
#            sub2 = sum(c_across(any_of(sometimes))), can.make3 = max(sub2 == n.ingredients, can.make2), 
#            n.vegan = sum(c_across(any_of(vegan))), is.vegan = n.vegan == n.ingredients, 
#            n.veg = sum(c_across(any_of(veg))), is.veg = n.veg == n.ingredients) %>%
#     group_by(recipe, variant)%>% 
#     mutate(makeability = 5 - (1+sum(c_across(contains("can.make")))),
#            makeability = ifelse(makeability==4, 5, makeability))%>% 
#     mutate(makeability = replace_na(makeability, 995))
# 
#   
#   make4 <- ING %>% filter(makeable ==1) %>% select(name, has) %>%
#     left_join(., 
#               MEAL%>%ungroup() %>% select(name = recipe, makeability) %>% group_by(name) %>% summarise(makeability = min(makeability, na.rm = TRUE)),
#               by = "name") %>% 
#     filter(makeability %in% 1:3, has == 0) %$% name
#   reaching <- c(sometimes, make4) %>% unique()
#   make4 <- setdiff(make4, c(subs1, subs2))
#   nh <- setdiff(nh, make4)
#   
#   
#   MEAL <- MEAL %>% 
#     mutate(makeability = replace_na(makeability, 997.1)) %>%
#     mutate(sub4 = sum(c_across(any_of(reaching))), can.make4 = 1*(sub4 == n.ingredients),
#                           makeability = ifelse(makeability==5 & can.make4==1, 4, makeability)) %>%
#     mutate(makeability = replace_na(makeability, 997.2)) %>%
#     group_by(recipe) %>%
#     mutate(variants = n(), make1 = sum(makeability==1), make2 = sum(makeability==2), make3 = sum(makeability==3)) %>%
#     mutate(makeability = replace_na(makeability, 997.3)) %>%
#     select(1, contains("make"), everything()) %>% 
#     arrange(recipe, makeability, desc(n.ingredients)) %>%
#     mutate(makeability = replace_na(makeability, 997.4)) %>%
#     select(Meal = recipe, variant,makeability, n.ingredients, contains("can.make"), n.vegan, is.vegan, n.veg, is.veg, everything())%>% 
#     mutate(makeability = replace_na(makeability, 997.5)) %>%
#     dplyr::slice(1) %>%
#     mutate(makeability = replace_na(makeability, 997.6)) %>%
#     left_join(MEALS %>% select(Meal, Ingredients), ., by = "Meal")%>%
#     mutate(makeability = replace_na(makeability, 997.7))
#   
#   
#   
#   MEAL %>% 
#     mutate(Ingredients = sprintj6(all_of(has), Ingredients, "#458B00"))%>%
#     mutate(Ingredients = sprintj6(all_of(nh), Ingredients, "#CD2626"))%>%
#     mutate(Ingredients = sprintj6(all_of(subs1), Ingredients, "#CDAD00"))%>%
#     mutate(Ingredients = sprintj6(all_of(subs2), Ingredients, "orange"))%>%
#     mutate(Ingredients = sprintj6(all_of(make4), Ingredients, "#1C86EE"))%>%
#     mutate(Ingredients = gsub("xyz", " or ", gsub("><",">, <",gsub("&","", Ingredients))) )%>%
#     mutate(makeability = replace_na(makeability, 997.8))%>%
#     left_join(MEALS %>% select(-Ingredients), ., by = "Meal")%>%
#     mutate(makeability = replace_na(makeability, 997.9))%>%
#     group_by(Meal) %>%
#     mutate(diet = case_when(is.vegan ~ "Vegan",
#                             is.veg & !is.vegan ~ "Vegetarian",
#                             TRUE ~ "Non-vegetarian"),
#            Needed = n.ingredients - p.ingredients) %>%
#     select(recipe = Meal,  Source, Page,Type, Cuisine,diet, Ingredients, makeability, Needed) %>%
#     mutate(makeability = replace_na(makeability, 999))
# }

update_ingredients <- function(DAT, ING){
  ## List of all ingredients used in each recipe
  ### ingredient list updated with new recipes
  DAT %>% group_by(Recipe) %>% summarise(across(where(is.numeric), max))%>% 
    pivot_longer(., -Recipe) %>% filter(value > 0) %>% group_by(name) %>% tally() %>% arrange(desc(n)) %>%
    full_join(., ING %>% select(-n), by = "name") %>%arrange(class, class2, desc(n)) %>%
    mutate(has = replace_na(has, 0), n = replace_na(n, 0))
}

f.optional2 <- function(vec) {ifelse(length(vec)==0, "", paste0("[", str_c(vec, collapse = " or "), "]"))}
f.exchange2 <- function(vec) {ifelse(length(vec)==0, "", paste0("(",gsub(" or [0-9]+ or", ",", paste0(str_c(vec, collapse = " or "), ")"))))}
f.include2 <- function(vec) {str_c(c(vec), collapse = ", ")}
# paste.ingredients2 <- function(opt, exc, inc) paste(f.include2(inc),f.exchange2(exc),f.optional2(opt), collapse = ",")

paste.ingredients2 <- function(opt, exc, inc){
  data.frame(ing = c(f.include2(inc), f.exchange2(exc), f.optional2(opt) ), 
             type = c("in", "ex", "op")) %>% 
    filter(ing != "") %$% toString(ing)
}
