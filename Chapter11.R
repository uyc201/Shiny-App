### Chapter 11 Notes & Exercise ###

#library
library(shiny)
library(dplyr, warn.conflicts = FALSE)


####################################### Notes #######################################
#you can’t bookmark the app to return to the same place in the future or share your work with someone else with a link in an email.
#That’s because, by default, Shiny does not expose the current state of the app in its URL.
#Fortunately, however, you can change this behaviour with a little extra work and this chapter will show you how.

#Let's say we want to make the following app 'bookmarkable'.
#This app draws Lissajous figures, which replicate the motion of a pendulum, and can produce a variety of interesting patterns:
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("omega", "omega", value = 1, min = -2, max = 2, step = 0.01),
      sliderInput("delta", "delta", value = 1, min = 0, max = 2, step = 0.01),
      sliderInput("damping", "damping", value = 1, min = 0.9, max = 1, step = 0.001),
      numericInput("length", "length", value = 100)
    ),
    mainPanel(
      plotOutput("fig")
    )
  )
)
server <- function(input, output, session) {
  t <- reactive(seq(0, input$length, length.out = input$length * 100))
  x <- reactive(sin(input$omega * t() + input$delta) * input$damping ^ t())
  y <- reactive(sin(t()) * input$damping ^ t())
  
  output$fig <- renderPlot({
    plot(x(), y(), axes = FALSE, xlab = "", ylab = "", type = "l", lwd = 2)
  }, res = 96)
}
shinyApp(ui, server)


#There are three things we need to do to make this app bookmarkable:
#1. Add a bookmarkButton() to the UI. This generates a button that the user clicks to generate the bookmarkable URL.
#2. Turn ui into a function. You need to do this because bookmarked apps have to replay the bookmarked values:
#   effectively, Shiny modifies the default value for each input control. This means there’s no longer a single static
#   UI but multiple possible UIs that depend on parameters in the URL; i.e. it has to be a function.
#3. Add enableBookmarking = "url" to the shinyApp() call.

#Instead of providing an explicit button, another option is to automatically update the URL in the browser.
#This allows your users to use the user bookmark command in their browser, or copy and paste the URL from the location bar.
#Automatically updating the URL requires a little boilerplate in the server function:
# Automatically bookmark every time an input changes
observe({
  reactiveValuesToList(input)
  session$doBookmark()
})
# Update the query string
onBookmarked(updateQueryString)

#Which gives us an updated server function as follows:
server <- function(input, output, session) {
  t <- reactive(seq(0, input$length, length = input$length * 100))
  x <- reactive(sin(input$omega * t() + input$delta) * input$damping ^ t())
  y <- reactive(sin(t()) * input$damping ^ t())
  
  output$fig <- renderPlot({
    plot(x(), y(), axes = FALSE, xlab = "", ylab = "", type = "l", lwd = 2)
  }, res = 96)
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
}
shinyApp(ui, server, enableBookmarking = "url")
#And this yields https://hadley.shinyapps.io/ms-bookmark-auto — since the URL now automatically updates,
#you could now remove the bookmark button from the UI.


#So far we’ve used enableBookmarking = "url" which stores the state directly in the URL.
#This a good place to start because it’s very simple and works everywhere you might deploy your Shiny app.
#As you can imagine, however, the URL is going to get very long if you have a large number of inputs, and
#it’s obviously not going to be able to capture an uploaded file.
#For these cases, you might instead want to use enableBookmarking = "server", which saves the state to an .rds file on the server.
#This always generates a short, opaque, URL but requires additional storage on the server:
#shinyApp(ui, server, enableBookmarking = "server")

#The main drawbacks with server bookmarking is that it requires files to be saved on the server,
#and it’s not obvious how long these need to hang around for.



