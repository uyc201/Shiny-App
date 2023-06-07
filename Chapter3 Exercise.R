### Chapter 3 Exercise ###

#library
library(shiny)



####################################### Section 3.3 #######################################
#1. Fix the simple errors found in each of the three server functions below. First try spotting the problem just by reading the code;
#then run the code to make sure you’ve fixed it.

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

shinyApp(ui, server1)

server2 <- function(input, output, server) {
  greeting <- reactive(paste0("Hello ", input$name))
  output$greeting <- renderText(greeting())
}

shinyApp(ui, server2)

server3 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

shinyApp(ui, server3)


#2. Draw the reactive graph for the following server functions:

server1 <- function(input, output, session) {
  c <- reactive(input$a + input$b)
  e <- reactive(c() + input$d)
  output$f <- renderText(e())
}
#Answer: a + b -> c + d -> e -> f

server2 <- function(input, output, session) {
  x <- reactive(input$x1 + input$x2 + input$x3)
  y <- reactive(input$y1 + input$y2)
  output$z <- renderText(x() / y())
}
#Answer: x1 + x2 + x3 -> x; y1 + y2 -> y; x + y -> z

server3 <- function(input, output, session) {
  d <- reactive(c() ^ input$d)
  a <- reactive(input$a * 10)
  c <- reactive(b() / input$c) 
  b <- reactive(a() + input$b)
}
#Answer: a + b -> c; a -> a() + b -> b


#3. Why will this code fail? Why are range() and var() bad names for reactive?
#Answer: This code will fail because R doesn't know if range() and var() are function calls or reactive names
#Answer: var() and range() are bad names for reactive, because they're already existing function names in R

var <- reactive(df[[input$var]])
range <- reactive(range(var(), na.rm = TRUE))


