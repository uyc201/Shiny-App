### Chapter 15 Notes & Exercise ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#There are three fundamental building blocks of reactive programming: reactive values, reactive expressions, and observers.
#There are two types of reactive values:
#1. A single reactive value, created by reactiveVal().
#2. A list of reactive values, created by reactiveValues().
#They have slightly different interfaces for getting and setting values:
x <- reactiveVal(10)
x()       # get
#> [1] 10
x(20)     # set
x()       # get
#> [1] 20

r <- reactiveValues(x = 10)
r$x       # get
#> [1] 10
r$x <- 20 # set
r$x       # get
#> [1] 20


#It’s unfortunate that these two similar objects have rather different interfaces,
#but there’s no way to standardise them. However, while they look different, they behave the same,
#so you can choose between them based on which syntax you prefer.

#Recall that a reactive has two important properties: it’s lazy and cached.
#This means that it only does work when it’s actually needed, and if called twice in a row, it returns the previous value.
#There are two important details that we haven’t yet covered:
#1) what reactive expressions do with errors, and 2) why on.exit() works inside of them.

#You can think of reactive(x()) as a shortcut for function() x(), automatically adding laziness and caching.
#This is mostly of importance if you want to understand how Shiny is implemented,
#but means that you can use functions that only work inside functions. The most useful of these is on.exit()
#which allows you to run code when a reactive expression finishes, regardless of whether the reactive successfully
#returns an error or fails with an error.

#Observers and outputs are terminal nodes in the reactive graph. They differ from reactive expressions in two important ways:
#1. They are eager and forgetful — they run as soon as they possibly can and they don’t remember their previous action.
#   This eagerness is “infectious” because if they use a reactive expression, that reactive expression will also be evaluated.
#2. The value returned by an observer is ignored because they are designed to work with functions called for their side-effects,
#   like cat() or write.csv().

#There are two important tools for controlling exactly how and when the reactive graph is invalidated.
#For this, we will discuss isolate(), the tool that powers observeEvent() and eventReactive(), and that lets
#you avoid creating reactive dependencies when not needed.
#We'll also discuss invalidateLater() which allows you to generate reactive invalidations on a schedule.

#Observers are often coupled with reactive values in order to track state changes over time.
#For example, take this code which tracks how many times x changes:
r <- reactiveValues(count = 0, x = 1)
observe({
  r$x
  r$count <- r$count + 1
})
#If you were to run it, you’d immediately get stuck in an infinite loop because the observer will take a
#reactive dependency on x and count; and since the observer modifies count, it will immediately re-run.
#Fortunately, Shiny provides isolate() to resolve this problem. This function allows you to access the current
#value of a reactive value or expression without taking a dependency on it:
r <- reactiveValues(count = 0, x = 1)
class(r)
#> [1] "rv_flush_on_write" "reactivevalues"
observe({
  r$x
  r$count <- isolate(r$count) + 1
})

r$x <- 1
r$x <- 2
r$count
#> [1] 2

r$x <- 3
r$count
#> [1] 3

#we can also use observeEvent() and eventReactive() similar to isolate()
#observeEvent() and eventReactive() have additional arguments that allow you to control the details of their operation:
#1. By default, both functions will ignore any event that yields NULL (or in the special case of action buttons, 0).
#   Use ignoreNULL = FALSE to also handle NULL values.
#2. By default both functions will run once when you create them. Use ignoreInit = TRUE to skip this run.
#3. For observeEvent() only, you can use once = TRUE to run the handler only once.
#Note: difference between observeEvent() and eventReactive() is like the difference between observe and reactive.
#      One is intended to be run when some reactive variable is "triggered" and is meant to have side effects (observeEvent),
#      and the other returns a reactive value and is meant to be used as a variable (eventReactive).
#      eventReactive creates a reactive value that changes based on the eventExpr while observeEvent simply is triggered based on eventExpr

#invalidateLater() lets you invalidate the reactive graph when no data has changed.
#invalidateLater(ms) causes any reactive consumer to be invalidated in the future, after ms milliseconds.
#It is useful for creating animations and connecting to data sources outside of Shiny’s reactive framework
#that may be changing over time. For example, this reactive will automatically generate 10 fresh random
#normals every half a second:
x <- reactive({
  invalidateLater(500)
  rnorm(10)
})


####################################### Section 15.4 #######################################
#1. Complete the app below with a server function that updates out with the value of x only when the button is pressed.
ui <- fluidPage(
  numericInput("x", "x", value = 50, min = 0, max = 100),
  actionButton("capture", "capture"),
  textOutput("out")
)
server <- function(input, output, session){
  observeEvent(input$capture, updateNumericInput(inputId="x", value=(input$x+1)))
  output$out <- renderText(paste0("The new value is: ", input$x))
}
shinyApp(ui, server)

