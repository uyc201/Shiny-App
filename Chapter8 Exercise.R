### Chapter 8 Exercise ###

#library
library(shiny)


####################################### Notes #######################################
#Validation: informating the user when an input (or combination of inputs) is in an invalid state
#Notification: sending general messages to the user
#Progress Bars: gives details for time consuming operations made up of many small steps

#A great way to give additional feedback to the user is via the shinyFeedback package
#First, add useShinyFeedback() to the ui. This sets up the needed HTML and JavaScript for attractive error message display:
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  numericInput("n", "n", value = 10),
  textOutput("half")
)
#The in the server function, call one of the feedback functions: feedback(), feedbackWarning(), feedbackDanger(), or feedbackSuccess()
#They all have three key arguments of inputId, show, and text
server <- function(input, output, session) {
  half <- reactive({
    even <- input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    input$n / 2    
  })
  
  output$half <- renderText(half())
}

#To stop inputs from triggering ractive changes, you need to use req()
server <- function(input, output, session) {
  half <- reactive({
    even <- input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    req(even)
    input$n / 2    
  })
  
  output$half <- renderText(half())
}
#When the input to req() is not true, it sends a special signal to tell Shiny that the reactive does not have all the inputs
#that it requires, so it should be 'paused'.
#req() works by signalling a special condition. This special condition causes all downstream reactives and outputs to stop executing.

#When called inside a reactive or an output, validate(message) stops execution of the rest of the code and instead displays message in any downstream outputs.
#The following code shows a simple example where we don’t want to log or square-root negative values:
ui <- fluidPage(
  numericInput("x", "x", value = 0),
  selectInput("trans", "transformation", 
              choices = c("square", "log", "square-root")
  ),
  textOutput("out")
)

server <- function(input, output, session) {
  output$out <- renderText({
    if (input$x < 0 && input$trans %in% c("log", "square-root")) {
      validate("x can not be negative for this transformation")
    }
    
    switch(input$trans,
           square = input$x ^ 2,
           "square-root" = sqrt(input$x),
           log = log(input$x)
    )
  })
}

#If there isn’t a problem and you just want to let the user know what’s happening, then you want a notification.
#You can create it with showNotification(), and there are three basic ways to use showNotification():
#1. To show a transient notification that automatically disappears after a fixed amount of time.
#2. To show a notification when a process starts and remove it when the process ends.
#3. To update a single notification with progressive updates.

#1. Transient Notification
#The simplest way to use showNotification() is to call it with a single argument: the message that you want to display.
ui <- fluidPage(
  actionButton("goodnight", "Good night")
)
server <- function(input, output, session) {
  observeEvent(input$goodnight, {
    showNotification("So long")
    Sys.sleep(1)
    showNotification("Farewell")
    Sys.sleep(1)
    showNotification("Auf Wiedersehen")
    Sys.sleep(1)
    showNotification("Adieu")
  })
}
#By default, the message will disappear after 5 seconds, which you can override by setting duration, or the user can dismiss it
#earlier by clicking the close button. If you want to make the notification more prominent, you can set the type argument to
#one of “message”, “warning”, or “error”.

#To remove a notification upon a completion of a task, set duration=NULL and closeButton=FALSE when creating notification.
#Then store id returned by showNotification(), and then pass this value to removeNotification().
#The most reliable way to do so is to use on.exit()
server <- function(input, output, session) {
  data <- reactive({
    id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    read.csv(input$file$datapath)
  })
}

#Instead of multiple calls to showNotification(), you can instead update a single notification by capturing the id from
#the first call and using it in subsequent calls.
#This is useful if your long-running task has multiple subcomponents.
ui <- fluidPage(
  tableOutput("data")
)

server <- function(input, output, session) {
  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
  }
  
  data <- reactive({ 
    id <- notify("Reading data...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    
    notify("Reticulating splines...", id = id)
    Sys.sleep(1)
    
    notify("Herding llamas...", id = id)
    Sys.sleep(1)
    
    notify("Orthogonalizing matrices...", id = id)
    Sys.sleep(1)
    
    mtcars
  })
  
  output$data <- renderTable(head(data()))
}


#For long-running tasks, the best type of feedback is a progress bar, but there is a major drawback:
#to use a progress bar you need to be able to divide the big task into a known number of small pieces that each take roughly the same amount of time.
#To create a progress bar with Shiny, you need to use withProgress() and incProgress():
ui <- fluidPage(
  numericInput("steps", "How many steps?", 10),
  actionButton("go", "go"),
  textOutput("result")
)

server <- function(input, output, session) {
  data <- eventReactive(input$go, {
    withProgress(message = "Computing random number", {
      for (i in seq_len(input$steps)) {
        Sys.sleep(0.5)
        incProgress(1 / input$steps)
      }
      runif(1)
    })
  })
  
  output$result <- renderText(round(data(), 2))
}

#The built-in progress bar is great for the basics, but if you want something that provides more visual options, you might try the waiter package.
server <- function(input, output, session) {
  data <- eventReactive(input$go, {
    waitress <- waiter::Waitress$new(max = input$steps)
    on.exit(waitress$close())
    
    for (i in seq_len(input$steps)) {
      Sys.sleep(0.5)
      waitress$inc(1)
    }
    
    runif(1)
  })
  
  output$result <- renderText(round(data(), 2))
}


#Sometimes you don’t know exactly how long an operation will take, and you just want to display an animated spinner that reassures the user that something is happening.
ui <- fluidPage(
  waiter::use_waiter(),
  actionButton("go", "go"),
  textOutput("result")
)

server <- function(input, output, session) {
  data <- eventReactive(input$go, {
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    Sys.sleep(sample(5, 1))
    runif(1)
  })
  output$result <- renderText(round(data(), 2))
}



#Sometimes an action is potentially dangerous, and you either want to make sure that the user really wants to do it,
#or you want to give them the ability to back out before it’s too late. The three techniques in this section lay out
#your basic options and give you some tips for how you might implement them in your app.

#1. Explicit Confirmation
#Create a dialog box with modalDialog()
server <- function(input, output, session) {
  observeEvent(input$delete, {
    showModal(modal_confirm)
  })
  
  observeEvent(input$ok, {
    showNotification("Files deleted")
    removeModal()
  })
  observeEvent(input$cancel, {
    removeModal()
  })
}
#where modal_confirm is:
modal_confirm <- modalDialog(
  "Are you sure you want to continue?",
  title = "Deleting files",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
  )
)

#2. Undoing an action
runLater <- function(action, seconds = 3) {
  observeEvent(
    invalidateLater(seconds * 1000), action, 
    ignoreInit = TRUE, 
    once = TRUE, 
    ignoreNULL = FALSE,
    autoDestroy = FALSE
  )
}

server <- function(input, output, session) {
  waiting <- NULL
  last_message <- NULL
  
  observeEvent(input$tweet, {
    notification <- glue::glue("Tweeted '{input$message}'")
    last_message <<- input$message
    updateTextAreaInput(session, "message", value = "")
    
    showNotification(
      notification,
      action = actionButton("undo", "Undo?"),
      duration = NULL,
      closeButton = FALSE,
      id = "tweeted",
      type = "warning"
    )
    
    waiting <<- runLater({
      cat("Actually sending tweet...\n")
      removeNotification("tweeted")
    })
  })
  
  observeEvent(input$undo, {
    waiting$destroy()
    showNotification("Tweet retracted", id = "tweeted")
    updateTextAreaInput(session, "message", value = last_message)
  })
}


#3. Trash
#For actions that you might regret days later, a more sophisticated pattern is to implement something like the
#trash or recycling bin on your computer. 




