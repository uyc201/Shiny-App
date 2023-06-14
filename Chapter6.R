### Chapter 6 Notes & Exercise ###

#library
library(shiny)
library(ggplot2)


####################################### Notes #######################################
#Layout functions provide the high-level visual structure of an app.
#fluidPage() sets up all the basic HTML, CSS, and JavaScript that Shiny needs.
#fixedPage() works like fluidPage() but has a fixed maximum width, which stops your apps from becoming unreasonable wide on bigger screens.
#fillPage() fills the full height of the browser and is useful if you want to make a plot that occupies the whole screen.
#fluidRow() creates a row in the page
#column() creates an object by the column width specified; each row is made up of 12 columns

#some layout functions that can be useful are:
#sidebarLayout() -> creates a chunk of area to place things like sidebarPanel and mainPanel side by side as 2 columns
#titlePanel() -> for something like app title/description
#sidebarPanel()
#mainPanel()

#tabPanel() creates the illusion of multiple pages
#The simple way to break up a page into pieces is to use tabsetPanel() and its close friend tabPanel().
#If you want to know what tab a user has selected, you can provide the id argument to tabsetPanel().

#navbarPage()and navbarMenu() provide two alternative layouts that let you use more tabs with longer titles.
#navlistPanel() is similar to tabsetPanel() but instead of running the tab titles horizontally, it shows them vertically in a sidebar.
#navbarPage() still runs the tab titles horizontally, but you can use navbarMenu() to add drop-down menus for an additional level of hierarchy

#Bootstrap is a collection of HTML conventions, CSS styles, and JS snippets bundled up into a convenient form.
#It is also one of the most popular CSS frameworks used on the web.
#With Boostrap you can:
#-You can use bslib::bs_theme() to customise the visual appearance of your code
#-You can use the class argument to customise some layouts, inputs, and outputs using Bootstrap class names
#-You can make your own functions to generate Bootstrap components that Shiny doesn’t provide
#But it is also possible to use a completely different CSS framework.

#To style your Shiny app, you can add theme using bslib package.
#To create theme we use:
#fluidPage(
# theme = bslib::bs_theme(...)
# )
#You can also customise your plots to match your app theme, which is really easy with thematic_shiny() in your server function.

#Though not required, if you know some HTML and CSS, it’s possible to customize Shiny still further.


####################################### Section 6.2 #######################################
#1.Read the documentation of sidebarLayout() to determine the width (in columns) of the sidebar and the main panel.
#Can you recreate its appearance using fluidRow() and column()? What are you missing?
#Answer: fluidRow(column(n_col,...)); with fluidRow(), it doesn't create boxes/frames around the output like sidebarLayout()

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel("Side Bar"),
    mainPanel("Main Panel")
  )
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)

ui2 <- fluidPage(
  titlePanel("Central limit theorem"),
  fluidRow(column(4, "Side Bar"),
            column(8, "Main Panel"))
)
shinyApp(ui2, server)


#2. Modify the Central Limit Theorem app to put the sidebar on the right instead of the left.

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    mainPanel(
      plotOutput("hist")
    ),
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}

shinyApp(ui, server)


#3. Create an app with that contains two plots, each of which takes up half of the width.
#Put the controls in a full width container below the plots.

ui <- fluidPage(
  fluidRow(column(6, plotOutput("first_plot")),
           column(6, plotOutput("second_plot"))),
  numericInput("x", "X Value", value = 1, min = 1, max = 20, width="100%")
)

server <- function(input, output, session){
  
  x_val <- reactive(input$x)
  y_val1 <- reactive(x_val() * 2)
  y_val2 <- reactive(x_val() * 4)
  
  output$first_plot <- renderPlot(plot(x_val(), y_val1()), res=96)
  output$second_plot <- renderPlot(plot(x_val(), y_val2()), res=96)
  
}

shinyApp(ui, server)


####################################### Section 6.5 #######################################
#1. Use bslib::bs_theme_preview() to make the ugliest theme you can.
#Run bslib::bs_theme_preview() to try out different theme options we have





