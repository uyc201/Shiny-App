### Chapter 18 Notes ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#In the UI, you have components that are repeated in multiple places with minor variations.
#Pulling out repeated code into a function reduces duplication (making it easier to update many controls from one place),
#and can be combined with functional programming techniques to generate many controls at once.

#In the server, complex reactives are hard to debug because you need to be in the midst of the app.
#Pulling out a reactive into a separate function, even if that function is only called in one place,
#makes it substantially easier to debug, because you can experiment with computation independent of reactivity.

#Functions have another important role in Shiny apps: they allow you to spread out your app code across multiple files.
#While you certainly can have one giant app.R file, it’s much easier to manage when spread across multiple files.

#Immediately we have one obvious benefit: functions can live outside of app.R. There are two places you might put them
#depending on how big they are:
#1. I recommend putting large functions (and any smaller helper functions that they need) into their own R/{function-name}.R file.
#2. You might want to collect smaller, simpler, functions into one place. I often use R/utils.R for this,
#   but if they’re primarily used in your ui you might use R/ui.R.

#Functions are a powerful tool to reduce duplication in your UI code. For example, for code below:
ui <- fluidRow(
  sliderInput("alpha", "alpha", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("beta",  "beta",  min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("gamma", "gamma", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("delta", "delta", min = 0, max = 1, value = 0.5, step = 0.1)
)
#we can change to something like this:
sliderInput01 <- function(id) { #a function to create slider input
  sliderInput(id, label = id, min = 0, max = 1, value = 0.5, step = 0.1)
}

ui <- fluidRow(
  sliderInput01("alpha"),
  sliderInput01("beta"),
  sliderInput01("gamma"),
  sliderInput01("delta")
)
#Here a function helps in two ways:
#1. We can give the function a evocative name, making it easier to understand what’s going on when we re-read the code in the future.
#2. If we need to change the behaviour, we only need to do it in one place. For example, if we decided that we needed a
#   finer resolution for the steps, we only need to write step = 0.01 in one place, not four.

#In functions, we can have ... to mean that you can still pass along any other arguments like in the example below:
usWeekDateInput <- function(inputId, ...) {
  dateInput(inputId, ..., format = "dd M, yy", daysofweekdisabled = c(0, 6))
}

#Returning back to our example above with function sliderInput01,
#you could reduce the code still further if you’re comfortable with functional programming.
library(purrr)

vars <- c("alpha", "beta", "gamma", "delta")
sliders <- map(vars, sliderInput01) #lapply() is the base equivalent to map
ui <- fluidRow(sliders)
#There are two big ideas here:
# 1. map() calls sliderInput01() once for each string stored in vars. It returns a list of sliders.
# 2. When you pass a list into fluidRow() (or any html container), it automatically unpacks the list
#    so that the elements become the children of the container.

#It’s possible to generalise this idea further if the controls have more than one varying input. First, we create an
#inline data frame that defines the parameters of each control, using tibble::tribble().
#We’re turning UI structure into an explicit data structure.
vars <- tibble::tribble(
  ~ id,   ~ min, ~ max,
  "alpha",     0,     1,
  "beta",      0,    10,
  "gamma",    -1,     1,
  "delta",     0,     1,
)
#Then we create a function where the argument names match the column names:
mySliderInput <- function(id, label = id, min = 0, max = 1) {
  sliderInput(id, label, min = min, max = max, value = 0.5, step = 0.1)
}
#Then finally we use purrr::pmap() to call mySliderInput() once for each row of vars:
sliders <- pmap(vars, mySliderInput)


#Whenever you have a long reactive (say >10 lines) you should consider pulling it out into a separate
#function that does not use any reactivity. This has two advantages:
#1. It is much easier to debug and test your code if you can partition it so that reactivity lives inside of server(),
#   and complex computation lives in your functions.
#2. When looking at a reactive expression or output, there’s no way to easily tell exactly what values it depends on,
#   except by carefully reading the code block. A function definition, however, tells you exactly what the inputs are.

#Reading Uploaded Data
#Take this server from Section 9.1.3. It contains a moderately complex reactive():
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}
#If this was a real app, I'd seriously consider extracting out a function specifically for reading uploaded files:
load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ","),
         tsv = vroom::vroom(path, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}
#Generally, I (the author) think it’s better to keep the reactive and non-reactive parts of your app as separate as possible.
#In this case, I’m still using validate(); that works because outside of Shiny validate() works similarly to stop().
#But I keep the req() in the server, because it shouldn’t be the responsibility of the file parsing code to know when it’s run.

#Since this is now an independent function, it could live in its own file (R/load_file.R, say), keeping the server() svelte.
#This helps keep the server function focused on the big picture of reactivity, rather than the smaller details
#underlying each component.
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    load_file(input$file$name, input$file$datapath)
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}

#Most of the time you’ll want to make the function completely independent of the server function so that you can put
#it in a separate file. However, if the function needs to use input, output, or session it may make sense for the
#function to live inside the server function:
server <- function(input, output, session) {
  switch_page <- function(i) {
    updateTabsetPanel(input = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
}


