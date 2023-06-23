### Chapter 19 Notes ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#Functions work well for code that is either completely on the server side or completely on the client side.
#For code that spans both, i.e. whether the server code relies on specific structure in the UI, you’ll need a new technique: modules.

#At the simplest level, a module is a pair of UI and server functions. The magic of modules comes because these
#functions are constructed in a special way that creates a “namespace”.
#So far, when writing an app, the names (ids) of the controls are global: all parts of your server function can see
#all parts of your UI. Modules give you the ability to create controls that can only be seen from within the module.
#This is called a namespace because it creates “spaces” of “names” that are isolated from the rest of the app.

#Shiny modules have two big advantages. Firstly, namespacing makes it easier to understand how your app works because you can write,
#analyse, and test individual components in isolation. Secondly, because modules are functions they help you reuse code;
#anything you can do with a function, you can do with a module.

#A module is a black box with defined inputs and outputs. Other modules can only communicate via the interface (outside) of a module,
#they can’t reach inside and directly inspect or modify the internal controls and reactives.
#This enforces a simpler structure to the whole app.

#Module Basics Example
#Below is a very simple app that draws a histogram:
ui <- fluidPage(
  selectInput("var", "Variable", names(mtcars)),
  numericInput("bins", "bins", 10, min = 1),
  plotOutput("hist")
)
server <- function(input, output, session) {
  data <- reactive(mtcars[[input$var]])
  output$hist <- renderPlot({
    hist(data(), breaks = input$bins, main = input$var)
  }, res = 96)
}
#A module is very similar to an app. Like an app, it’s composed of two pieces:
#1. The module UI function that generates the ui specification.
#2. The module server function that runs code inside the server function.

#We'll take the above example to create our own module, starting with the module UI which has two steps:
#1. Put the UI code inside a function that has an id argument.
#2. Wrap each existing ID in a call to NS(), so that (e.g.) "var" turns into NS(id, "var").
histogramUI <- function(id) {
  tagList( #this is a special type of layout function that allows you to bundle together multiple components without actually implying how they’ll be laid out
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}
#with tagList(), It’s the responsibility of the person calling histogramUI() to wrap the result in a layout function like column()
#or fluidRow() according to their needs.

#The server function gets wrapped inside another function which must have an id argument. This function calls moduleServer()
#with the id, and a function that looks like a regular server function. The two levels of functions are important here.
#We’ll come back to them later, but in short they help distinguish the argument to your module from the arguments
#to the server function:
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) { #Note that moduleServer() takes care of the namespacing automatically: 
    data <- reactive(mtcars[[input$var]]) #inside of moduleServer(id), input$var and input$bins refer to the inputs with names 
    output$hist <- renderPlot({ #NS(id, "var") and NS(id, "bins").
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}

#Now that we have the ui and server functions, we can write a function that uses them to generate an app
#which we can use for experimentation and testing:
histogramApp <- function() {
  ui <- fluidPage(
    histogramUI("hist1") #histogram module ui
  )
  server <- function(input, output, session) {
    histogramServer("hist1") #histogram module server; you need to use the same id in both UI and server, otherwise the two pieces will not be connected
  }
  shinyApp(ui, server) #run the app
}


#The key idea that makes modules work is that the name of each control (i.e. its id) is now determined by two pieces:
#1. The first piece comes from the module user, the developer who calls histogramServer().
#2. The second piece comes from the module author, the developer who wrote histogramServer().

#Namespacing turns modules into black boxes. From outside of the module, you can’t see any of the inputs, outputs,
#or reactives inside of it. For example, take the app below. The text output output$out will never get updated
#because there is no input$bins; the bins input can only be seen inside of the hist1 module.
ui <- fluidPage(
  histogramUI("hist1"),
  textOutput("out")
)
server <- function(input, output, session) {
  histogramServer("hist1")
  output$out <- renderText(paste0("Bins: ", input$bins))
}

#Note that the module UI and server differ in how the namespacing is expressed:
#In the module UI, the namespacing is explicit: you have to call NS(id, "name") every time you create an input or output.
#In the module server, the namespacing is implicit. You only need to use id in the call to moduleServer() and then Shiny
#automatically namespaces input and output so that in your module code input$name means the input with name NS(id, "name").

#Naming Conventions
#In the example above, we've created a module that creates histogram, so we've named it 'histogram module'.
#This base name is then used in variety of places:
#- R/histogram.R holds all the code for the module.
#- histogramUI() is the module UI. If it’s used primarily for input or output I’d call it histogramInput() or histogramOuput() instead.
#- histogramServer() is the module server.
#- histogramApp() creates a complete app for interactive experimentation and more formal testing.



####################################### Case Study: selecting a numeric variable #######################################
#We’ll create a control that allows the user to select variables of specified type from a given reactive dataset.

selectVarInput <- function(id) {
  selectInput(NS(id, "var"), "Variable", choices = NULL) 
}
#The server function will have two arguments:
#1. The data to select variables from. I want this to be reactive so it can work with the dataset module I created above.
#2. A filter used to select which variables to list. This will be set by the caller of the module, so doesn’t need to be reactive.
#   To keep the module server simple, I’ve extracted out the key idea into a helper function:
find_vars <- function(data, filter) {
  names(data)[vapply(data, filter, logical(1))]
}
#Then the module server uses observeEvent() to update the inputSelect choices when the data changes, and returns a
#reactive that provides the values of the selected variable.
selectVarServer <- function(id, data, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })
    
    reactive(data()[[input$var]])
  })
}
#To make our app, we again capture the results of the module server and connect it to an output in our UI.
#I want to make sure all the reactive plumbing is correct, so I use the dataset module as a source of reactive data frames.
selectVarApp <- function(filter = is.numeric) {
  ui <- fluidPage(
    datasetInput("data", is.data.frame),
    selectVarInput("var"),
    verbatimTextOutput("out")
  )
  server <- function(input, output, session) {
    data <- datasetServer("data")
    var <- selectVarServer("var", data, filter = filter)
    output$out <- renderPrint(var())
  }
  
  shinyApp(ui, server)
}



#it may make sense to create a module that itself contains a module. For example, we could combine the dataset and
#selectVar modules to make a module that allows the user to pick a variable from a built-in dataset:
selectDataVarUI <- function(id) {
  tagList(
    datasetInput(NS(id, "data"), filter = is.data.frame),
    selectVarInput(NS(id, "var"))
  )
}
selectDataVarServer <- function(id, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    data <- datasetServer("data")
    var <- selectVarServer("var", data, filter = filter)
    var
  })
}

selectDataVarApp <- function(filter = is.numeric) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(selectDataVarUI("var")),
      mainPanel(verbatimTextOutput("out"))
    )
  )
  server <- function(input, output, session) {
    var <- selectDataVarServer("var", filter)
    output$out <- renderPrint(var(), width = 40)
  }
  shinyApp(ui, server)
}


##Left here on Friday 6/23 @ 19.3.5 Case Sstudy: histogram


