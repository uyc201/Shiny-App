### Chapter 16 Notes & Exercise ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#There are two other important cases where you as the app author might invalidate an input:
#1. You call an update function setting the value argument. This sends a message to the browser to change the value of an input,
#   which then notifies R that the input value has been changed.
#2. You modify the value of a reactive value (created with reactiveVal() or reactiveValues()).

#It’s important to understand that in both of these cases a reactive dependency is not created between the reactive value and
#the observer. While these actions cause the graph to invalidate, they are not recorded through new connections.


####################################### Case Studies #######################################
#Next, lets take a look at a few useful cases where you might combine reactiveValues() and observeEvent() or
#observe() to solve problems that are otherwise very challenging (if not impossible). These are useful templates for your own apps.

#1. One output modified by multiple inputs: common text box that's updated by multiple events.
ui <- fluidPage(
  actionButton("drink", "drink me"),
  actionButton("eat", "eat me"),
  textOutput("notice")
)
server <- function(input, output, session) {
  r <- reactiveValues(notice = "")
  observeEvent(input$drink, {
    r$notice <- "You are no longer thirsty"
  })
  observeEvent(input$eat, {
    r$notice <- "You are no longer hungry"
  })
  output$notice <- renderText(r$notice)
}

#2. Things get slightly more complicated in the next example, where we have an app with two buttons that let you increase and
#decrease values. We use a reactiveValues() to store the current value, and then use observeEvent() to increment and decrement
#the value when the appropriate button is pushed. The main additional complexity here is that the new value of r$n depends on
#the previous value.
ui <- fluidPage(
  actionButton("up", "up"),
  actionButton("down", "down"),
  textOutput("n")
)
server <- function(input, output, session) {
  r <- reactiveValues(n = 0)
  observeEvent(input$up, {
    r$n <- r$n + 1
  })
  observeEvent(input$down, {
    r$n <- r$n - 1
  })
  
  output$n <- renderText(r$n)
}


#3. Accumulating inputs
ui <- fluidPage(
  textInput("name", "name"),
  actionButton("add", "add"),
  textOutput("names")
)
server <- function(input, output, session) {
  r <- reactiveValues(names = character())
  observeEvent(input$add, {
    r$names <- c(input$name, r$names)
    updateTextInput(session, "name", value = "") #we use updateTextInput() to reset the text box after the user clicks the add button
  })
  
  output$names <- renderText(r$names)
}
#We could make this slightly more useful by providing a delete button and making sure that the
#add button doesn’t create duplicate names:
ui <- fluidPage(
  textInput("name", "name"),
  actionButton("add", "add"),
  actionButton("del", "delete"),
  textOutput("names")
)
server <- function(input, output, session) {
  r <- reactiveValues(names = character())
  observeEvent(input$add, {
    r$names <- union(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })
  observeEvent(input$del, {
    r$names <- setdiff(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })
  
  output$names <- renderText(r$names)
}


#4. Pausing animations
# #Another common use case is to provide a start and stop button that lets you control some recurring event.
#This example uses a running reactive value to control whether or not the number increments,
#and invalidateLater() to ensure that the observer is invalidated every 250 ms when running.
ui <- fluidPage(
  actionButton("start", "start"),
  actionButton("stop", "stop"),
  textOutput("n")
)
server <- function(input, output, session) {
  r <- reactiveValues(running = FALSE, n = 0)
  
  observeEvent(input$start, {
    r$running <- TRUE
  })
  observeEvent(input$stop, {
    r$running <- FALSE
  })
  
  observe({
    if (r$running) {
      r$n <- isolate(r$n) + 1 #Notice in this case we can’t easily use observeEvent() because we perform different actions depending on whether running() is TRUE or FALSE. Since we can’t use observeEvent(), we must use isolate() — if we don’t this observer would also take a reactive dependency on n, which it updates, so it would get stuck in an infinite loop.
      invalidateLater(250)
    }
  })
  output$n <- renderText(r$n)
}


####################################### Chapter 16 Exercises #######################################
#1. Provide a server function that draws a histogram of 100 random numbers from a normal distribution
#when normal is clicked, and 100 random uniforms.
ui <- fluidPage(
  actionButton("rnorm", "Normal"),
  actionButton("runif", "Uniform"),
  plotOutput("plot")
)
server <- function(input, output, session){
  
  observeEvent(input$rnorm, {
    output$plot <- renderPlot(plot(rnorm(100)))
  })
  
  observeEvent(input$runif, {
    output$plot <- renderPlot(plot(runif(100)))
  })
}
shinyApp(ui, server)



#2. Modify your code from above for to work with this UI:
ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)
server <- function(input, output, session){
  observeEvent(input$go, {
    if(input$type=="Normal"){
      output$plot <- renderPlot(plot(rnorm(100)))
    }else if(input$type=="Uniform"){
      output$plot <- renderPlot(plot(runif(100)))
    }
  })
}
shinyApp(ui, server)

#3. Rewrite your code from the previous answer to eliminate the use of observe()/observeEvent() and only use reactive().
#Why can you do that for the second UI but not the first?
ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)
server <- function(input, output, session){
  go2 <- reactive(input$actionButton)
  
  r <- reactive(
    if((input$type=="Normal")&(go2())){
      plot(rnorm(100))
    }else if((input$type=="Uniform")&(go2())){
      plot(runif(100))
    }
  )
  
  output$plot <- renderPlot(r())
}
shinyApp(ui, server)







  