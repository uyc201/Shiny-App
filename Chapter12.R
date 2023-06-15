### Chapter 12 Notes & Exercise ###

#library
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


####################################### Notes #######################################
#If you are using Shiny with the tidyverse, you will almost certainly encounter the challenge of programming with tidy evaluation.
#In this chapter, you’ll learn how to wrap ggplot2 and dplyr function in a Shiny app.
#The techniques for wrapping ggplot2 and dplyr functions in a functions and package,
#are a a little different and covered in other resources like Using ggplot2 in packages or Programming with dplyr:
#https://ggplot2.tidyverse.org/dev/articles/ggplot2-in-packages.html


#In the example below, the app runs without an error, but there is a big flaw:
num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")
ui <- fluidPage(
  selectInput("var", "Variable", choices = num_vars),
  numericInput("min", "Minimum", value = 1),
  tableOutput("output")
)
server <- function(input, output, session) {
  data <- reactive(diamonds %>% filter(input$var > input$min)) #here, dplyr thinks you're asking for "Variable" > #
  output$output <- renderTable(head(data()))
}

#We can disambiguate the two uses between Variable and character "Variable" by introducing two new terms:
#1. An env-variable (environment variable) is a “programming” variables that you create with <-. input$var is a env-variable.
#2. A data-variable (data frame variables) is “statistical” variable that lives inside a data frame. carat is a data-variable.

#So in the example above, we have a data-variable (Variable) stored inside an env-variable (input$var) and we need some way to tell dplyr this.
#There are two slightly different ways to do this depending on whether the function you’re working with is a “data-masking” function or a “tidy-selection” function.

#1. Data-Masking
#Data-masking functions allow you to use variables in the “current” data frame without any extra syntax.
#It’s used in many dplyr functions like arrange(), filter(), group_by(), mutate(), and summarise(), and in ggplot2’s aes().
#Data-masking is useful because it lets you use data-variables without any additional syntax:
min <- 1
diamonds %>% filter(carat > min)
#compared to the base R equivalent: diamonds[diamonds$carat > min, ]

#Fortunately, inside data-masking functions you can use .data or .env if you want to be explicit about whether
#you’re talking about a data-variable or an env-variable:
diamonds %>% filter(.data$carat > .env$min)
#and with var <- "carat" we can switch $ to [[:
diamonds %>% filter(.data[[var]] > .env$min)

#With this, the above example can be changed to the following:
num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")
ui <- fluidPage(
  selectInput("var", "Variable", choices = num_vars),
  numericInput("min", "Minimum", value = 1),
  tableOutput("output")
)
server <- function(input, output, session) {
  data <- reactive(diamonds %>% filter(.data[[input$var]] > .env$input$min))
  output$output <- renderTable(head(data()))
}

#The same technique also works for dplyr.
#The following app extends the previous simple example to allow you to choose a variable to filter,
#a minimum value to select, and a variable to sort by:
ui <- fluidPage(
  selectInput("var", "Select variable", choices = names(mtcars)),
  sliderInput("min", "Minimum value", 0, min = 0, max = 100),
  selectInput("sort", "Sort by", choices = names(mtcars)),
  tableOutput("data")
)
server <- function(input, output, session) {
  observeEvent(input$var, {
    rng <- range(mtcars[[input$var]])
    updateSliderInput(
      session, "min", 
      value = rng[[1]], 
      min = rng[[1]], 
      max = rng[[2]]
    )
  })
  
  output$data <- renderTable({
    mtcars %>% 
      filter(.data[[input$var]] > input$min) %>% 
      arrange(.data[[input$sort]])
  })
}

#Most other problems can be solved by combining .data with your existing programming skills.

#Before we move on to talk about tidy-selection, there’s one last topic we need to discuss: user supplied data.
#Take this app shown below: it allows the user to upload a tsv file, then select a variable and filter by it.
#It will work for the vast majority of inputs that you might try it with.
ui <- fluidPage(
  fileInput("data", "dataset", accept = ".tsv"),
  selectInput("var", "var", character()),
  numericInput("min", "min", 1, min = 0, step = 1),
  tableOutput("output")
)
server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    vroom::vroom(input$data$datapath)
  })
  observeEvent(data(), {
    updateSelectInput(session, "var", choices = names(data()))
  })
  observeEvent(input$var, {
    val <- data()[[input$var]]
    updateNumericInput(session, "min", value = min(val))
  })
  
  output$output <- renderTable({
    req(input$var)
    
    data() %>% 
      filter(.data[[input$var]] > input$min) %>%  #There is a subtle problem with the use of filter() here.
      arrange(.data[[input$var]]) %>% 
      head(10)
  })
}

#Let’s pull the call to filter() so we can play around with it directly, outside of the app:
df <- data.frame(x = 1, y = 2)
input <- list(var = "x", min = 0)

df %>% filter(.data[[input$var]] > input$min)
#>   x y
#> 1 1 2
#there’s a subtle issue: what happens if the data frame contains a variable called input?
df <- data.frame(x = 1, y = 2, input = 3)
df %>% filter(.data[[input$var]] > input$min)
#> Error in `filter()`:
#> ! Problem while computing `..1 = .data[["x"]] > input$min`.
#> Caused by error in `input$min`:
#> ! $ operator is invalid for atomic vectors

#We get an error message because filter() is attempting to evaluate df$input$min:
df$input$min
#> Error in df$input$min: $ operator is invalid for atomic vectors
#This problem is due to the ambiguity of data-variables and env-variables, and because data-masking prefers to use a data-variable
#if both are available. We can resolve the problem by using .env38 to tell filter() only look for min in the env-variables:
df %>% filter(.data[[input$var]] > .env$input$min)
#>   x y input
#> 1 1 2     3
#Note that you only need to worry about this problem when working with user supplied data;
#when working with your own data, you can ensure the names of your data-variables don’t clash with the names of
#your env-variables (and if they accidentally do, you’ll discover it right away).


#As well as data-masking, there’s one other important part of tidy evaluation:
#tidy-selection. Tidy-selection provides a concise way of selecting columns by position, name, or type.
#It’s used in dplyr::select() and dplyr::across(), and in many functions from tidyr,
#like pivot_longer(), pivot_wider(), separate(), extract(), and unite().

#To refer to variables indirectly use any_of() or all_of()39: both expect a character vector env-variable containing the
#names of data-variables. The only difference is what happens if you supply a variable name that doesn’t exist in the input:
#all_of() will throw an error, while any_of() will silently ignore it.

#Working with multiple variables is trivial when you’re working with a function that uses tidy-selection:
#you can just pass a character vector of variable names into any_of() or all_of().
#Wouldn’t it be nice if we could do that in data-masking functions too? That’s the idea of the across() function,
#added in dplyr 1.0.0. It allows you to use tidy-selection inside data-masking functions.
#across() is typically used with either one or two arguments. The first argument selects variables,
#and is useful in functions like group_by() or distinct(). For example, the following app allows you to select
#any number of variables and count their unique values.
ui <- fluidPage(
  selectInput("vars", "Variables", names(mtcars), multiple = TRUE),
  tableOutput("count")
)

server <- function(input, output, session) {
  output$count <- renderTable({
    req(input$vars)
    
    mtcars %>% 
      group_by(across(all_of(input$vars))) %>% 
      summarise(n = n(), .groups = "drop")
  })
}

#The second argument is a function (or list of functions) that’s applied to each selected column.
#That makes it a good fit for mutate() and summarise() where you typically want to transform each variable in some way.
#For example, the following code lets the user select any number of grouping variables, and any number of variables to
#summarise with their means.
ui <- fluidPage(
  selectInput("vars_g", "Group by", names(mtcars), multiple = TRUE),
  selectInput("vars_s", "Summarise", names(mtcars), multiple = TRUE),
  tableOutput("data")
)

server <- function(input, output, session) {
  output$data <- renderTable({
    mtcars %>% 
      group_by(across(all_of(input$vars_g))) %>% 
      summarise(across(all_of(input$vars_s), mean), n = n())
  })
}

