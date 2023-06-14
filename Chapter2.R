### Chapter 2 Notes & Exercise ###

#library
library(shiny)
library(reactable)



####################################### Section 2.2 #######################################
#1. When space is at a premium, it’s useful to label text boxes using a placeholder that appears inside the text entry area.
#How do you call textInput() to generate the UI below?
#Answer: textInput("name", placeholder = "Your name")

ui <- fluidPage(
  textInput("name", "", placeholder = "Your name")
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)


#2. Carefully read the documentation for sliderInput() to figure out how to create a date slider, as shown below.
#Answer: sliderInput("delivery_date", "When should we deliver?", min="2020-09-16", max="2020-09-23", value="2020-09-17")

ui <- fluidPage(
  sliderInput("delivery_date", "When should we deliver?", min=as.Date("2020-09-16"), max=as.Date("2020-09-23"), value=as.Date("2020-09-17"))
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)



#3. Create a slider input to select values between 0 and 100 where the interval between each selectable value on the slider is 5.
#Then, add animation to the input widget so when the user presses play the input widget scrolls through the range automatically.

ui <- fluidPage(
  sliderInput("by5", label="", value=1, min=0, max=100, step=5, animate=TRUE)
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)


#4. If you have a moderately long list in a selectInput(), it’s useful to create sub-headings that break the list up into pieces.
#Read the documentation to figure out how. (Hint: the underlying HTML is called <optgroup>.)

#example below
shinyApp(
  ui = fluidPage(
    selectInput("state", "Choose a state:",
                list(`East Coast` = list("NY", "NJ", "CT"),
                     `West Coast` = list("WA", "OR", "CA"),
                     `Midwest` = list("MN", "WI", "IA"))
    ),
    textOutput("result")
  ),
  server = function(input, output) {
    output$result <- renderText({
      paste("You chose", input$state)
    })
  }
)
}



####################################### Section 2.3 #######################################
#1. Which of textOutput() and verbatimTextOutput() should each of the following render functions be paired with?
#a. renderPrint(summary(mtcars) -> verbatimTextOutput()
#b. renderText("Good morning!") -> textOutput()
#c. renderPrint(t.test(1:5, 2:6)) -> verbatimTextOutput()
#d. renderText(str(lm(mpg ~ wt, data = mtcars))) -> textOutput()



#2. Re-create the Shiny app from Section 2.3.3, this time setting height to 300px and width to 700px.
#Set the plot “alt” text so that a visually impaired user can tell that its a scatterplot of five random numbers.

ui <- fluidPage(
  plotOutput("plot", width = "700px", height="300px")
)
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96, alt="Scatterplot of 5 random numbers")
}

shinyApp(ui, server)



#3. Update the options in the call to renderDataTable() below so that the data is displayed, but all other controls are suppress
#(i.e. remove the search, ordering, and filtering commands). You’ll need to read
#?renderDataTable and review the options at https://datatables.net/reference/option/.

ui <- fluidPage(
  dataTableOutput("table")
)
server <- function(input, output, session) {
  output$table <- renderDataTable(mtcars, options = list(pageLength = 5, dom="t", searchable = FALSE))
}

shinyApp(ui, server)


#4. Alternatively, read up on reactable, and convert the above app to use it instead.

ui <- fluidPage(
  reactableOutput("table")
)
server <- function(input, output, session) {
  output$table <- renderReactable(reactable(mtcars))
}

shinyApp(ui, server)

