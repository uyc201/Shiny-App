### Chapter 10 Notes & Exercise ###

#library
library(shiny)
library(dplyr, warn.conflicts = FALSE)


####################################### Notes #######################################
#Dynamic User Interfaces: changing the UI using code run in the server function
#Three key techniques for creating dynamic user interfaces:
#1. Using the update family of functions to modify parameters of input controls
#2. Using tabsetPanel() to conditionally show and hide parts of the user interface
#3. Using uiOutput() and renderUI() to generate selected parts of the user interface with code

#textInput() and updateTextInput()
ui <- fluidPage(
  numericInput("min", "Minimum", 0),
  numericInput("max", "Maximum", 3),
  sliderInput("n", "n", min = 0, max = 3, value = 1)
)
server <- function(input, output, session) {
  observeEvent(input$min, { #the observeEvent() triggers updateSliderInput() whenever min/max input changes
    updateSliderInput(inputId = "n", min = input$min)
  })  
  observeEvent(input$max, {
    updateSliderInput(inputId = "n", max = input$max)
  })
}
shinyApp(ui, server)


#The simplest uses of the update functions are to provide small conveniences for the user.
#For example, maybe you want to make it easy to reset parameters back to their initial value.
#The following snippet shows how you might combine an actionButton(), observeEvent() and updateSliderInput():
ui <- fluidPage(
  sliderInput("x1", "x1", 0, min = -10, max = 10),
  sliderInput("x2", "x2", 0, min = -10, max = 10),
  sliderInput("x3", "x3", 0, min = -10, max = 10),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(inputId = "x1", value = 0)
    updateSliderInput(inputId = "x2", value = 0)
    updateSliderInput(inputId = "x3", value = 0)
  })
}

#with actionButton() we can also use updateActionButton()

#Hierarchical select boxes: Example
sales <- vroom::vroom("sales_data_sample.csv", col_types = list(), na = "") #read in data
sales %>% #see what data looks like
  select(TERRITORY, CUSTOMERNAME, ORDERNUMBER, everything()) %>%
  arrange(ORDERNUMBER) %>% View()
#with this data, we want to create a user interface where you can:
#select a territory to see all customers, and select a customer to see all orders, and then select an order to see the underlying rows:
ui <- fluidPage(
  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
  selectInput("customername", "Customer", choices = NULL),
  selectInput("ordernumber", "Order number", choices = NULL),
  tableOutput("data")
)
server <- function(input, output, session) {
  territory <- reactive({
    filter(sales, TERRITORY == input$territory)
  })
  observeEvent(territory(), { #if territory selected, update list of choices for customer select box
    choices <- unique(territory()$CUSTOMERNAME)
    updateSelectInput(inputId = "customername", choices = choices) 
  })
  
  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
  })
  observeEvent(customer(), { #if customer selected, update list of choices for order select box
    choices <- unique(customer()$ORDERNUMBER)
    updateSelectInput(inputId = "ordernumber", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$ordernumber)
    customer() %>% #display the selected orders
      filter(ORDERNUMBER == input$ordernumber) %>% 
      select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
  })
}

#Freezing reactive inputs
#freezeReactiveValue() ensures that any reactives or outputs that use the input won’t be updated until the next full round of invalidation
server <- function(input, output, session) {
  dataset <- reactive(get(input$dataset, "package:datasets"))
  
  observeEvent(input$dataset, {
    freezeReactiveValue(input, "column")
    updateSelectInput(inputId = "column", choices = names(dataset()))
  })
  
  output$summary <- renderPrint({
    summary(dataset()[[input$column]])
  })
}
#You might wonder when you should use freezeReactiveValue(): it’s actually good practice to always use it when you dynamically change an input value. 


#Circular References
#In the following app, it contains a single input control and an observer that increments its value by one.
#Every time updateNumericInput() runs, it changes input$n, causing updateNumericInput() to run again,
#so the app gets stuck in an infinite loop constantly increasing the value of input$n.
ui <- fluidPage(
  numericInput("n", "n", 0)
)
server <- function(input, output, session) {
  observeEvent(input$n,
               updateNumericInput(inputId = "n", value = input$n + 1)
  )
}

#One place where it’s easy to end up with circular references is when you have multiple “sources of truth” in an app.
#For example, imagine that you want to create a temperature conversion app where you can either enter the temperature in
#Celsius or in Fahrenheit:
ui <- fluidPage(
  numericInput("temp_c", "Celsius", NA, step = 1),
  numericInput("temp_f", "Fahrenheit", NA, step = 1)
)

server <- function(input, output, session) {
  observeEvent(input$temp_f, {
    c <- round((input$temp_f - 32) * 5 / 9)
    updateNumericInput(inputId = "temp_c", value = c)
  })
  
  observeEvent(input$temp_c, {
    f <- round((input$temp_c * 9 / 5) + 32)
    updateNumericInput(inputId = "temp_f", value = f)
  })
}
#the full app above can be found here:  https://hadley.shinyapps.io/ms-temperature
#issue with this app:
#Set 120 F, then click the down arrow.
#F changes to 119, and C is updated to 48.
#48 C converts to 118 F, so F changes again to 118.
#Fortunately 118 F is still 48 C, so the updates stop there.



#You can use tabsetPanel() to create panels, as well as sidebarLayout() and sidebarPanel()
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("controller", "Show", choices = paste0("panel", 1:3))
    ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller) #switch tabs from the server
  })
}


#Using tabsetPanel and tabPanel():
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean", "mean", value = 1),
           numericInput("sd", "standard deviation", min = 0, value = 1)
  ),
  tabPanel("uniform", 
           numericInput("min", "min", value = 0),
           numericInput("max", "max", value = 1)
  ),
  tabPanel("exponential",
           numericInput("rate", "rate", value = 1, min = 0),
  )
)
#now embed this inside a fuller UI:
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution", 
                  choices = c("normal", "uniform", "exponential")
      ),
      numericInput("n", "Number of samples", value = 100),
      parameter_tabs,
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  }) 
  
  sample <- reactive({
    switch(input$dist, #switch evaluates expression accordingly
           normal = rnorm(input$n, input$mean, input$sd),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
    )
  })
  output$hist <- renderPlot(hist(sample()), res = 96)
}
shinyApp(ui, server)



#Sometimes you need to create different types or numbers of inputs (or outputs), depending on other inputs.
#This technique gives you the ability to create and modify the user interface while the app is running.
#There are two parts to this solution:
#1. uiOutput() inserts a placeholder in your ui. This leaves a “hole” that your server code can later fill in.
#2. renderUI() is called within server() to fill in the placeholder with dynamically generated UI.
#Simple Example:
ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    value <- isolate(input$dynamic) #this ensures we don't create a reactive dependency that would cause the code to re-run every time input$dynamic changes
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = value, min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = value, min = 0, max = 10)
    }
  })
}


#Example:
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of colours", value = 5, min = 1),
      uiOutput("col"),
    ),
    mainPanel(
      plotOutput("plot")  
    )
  )
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))
  
  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])))
  })
  
  output$plot <- renderPlot({
    cols <- map_chr(col_names(), ~ input[[.x]] %||% "")
    # convert empty inputs to transparent
    cols[cols == ""] <- NA
    
    barplot(
      rep(1, length(cols)), 
      col = cols,
      space = 0, 
      axes = FALSE
    )
  }, res = 96)
}



####################################### Section 10.1 #######################################
#1. Complete the user interface below with a server function that updates input$date so that you can only select dates in input$year.
ui <- fluidPage(
  numericInput("year", "year", value = 2020, min=1990, max=2050),
  dateInput("date", label="date", value=NULL)
)
server <- function(input, output, session){
  observeEvent(input$year, {
    req(input$date, input$year)
    updateDateInput("date", value=ymd(paste0(input$year,"-01-01")), min=ymd(paste0(input$year,"-01-01")), max=ymd(paste0(input$year,"-12-31")))
  })
}
shinyApp(ui, server)


#2. Complete the user interface below with a server function that updates input$county choices based on input$state.
#For an added challenge, also change the label from “County” to “Parish” for Louisiana and “Borough” for Alaska.
library(openintro, warn.conflicts = FALSE)
states <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "Select State", choices = states),
  selectInput("my_county", "Select County", choices = NULL),
  tableOutput("state_table")
)

server <- function(input, output, session){
  chosen_state <- reactive({
    county %>% dplyr::filter(state == input$state)
    })

  observeEvent(chosen_state(), {
    county_choies <- unique(chosen_state()$name)
    updateSelectInput(inputId="my_county", choices=county_choies)
  })
  
  observeEvent(chosen_state(), {
    if(input$state=="Louisiana"){
      updateSelectInput(inputId="my_county", label="Select Parish")
    } else if(input$state=="Alaska"){
      updateSelectInput(inputId="my_county", label="Select Borough")
      } else{
          updateSelectInput(inputId="my_county", label="Select County")
        }
      }
)
  
  output$state_table <- renderTable({
    req(input$my_county)
    chosen_state() %>% filter(name == input$my_county)
    })
}
shinyApp(ui, server)


#3. Complete the user interface below with a server function that updates input$country choices based on the input$continent.
#Use output$data to display all matching rows.
library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)
server <- function(input, output, session){
  selected_continent <- reactive({
    gapminder %>% filter(continent==input$continent)
  })
  
  observeEvent(selected_continent(), {
    country_choices=unique(selected_continent()$country)
    updateSelectInput(inputId="country", choices=country_choices)
  })
  
  output$data <- renderTable({
    req(input$country)
    selected_continent() %>% filter(country==input$country)
  })
  
}
shinyApp(ui, server)


#4.Extend the previous app so that you can also choose to select all continents, and hence see all countries.
#You’ll need to add "(All)" to the list of choices, and then handle that specially when filtering.
library(gapminder)
continents <- c("All", as.character(unique(gapminder$continent)))

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)
server <- function(input, output, session){
  selected_continent <- reactive({
    if(input$continent=="All"){
      gapminder
    }else{
      gapminder %>% filter(continent==input$continent)
    }
  })
  
  observeEvent(selected_continent(), {
    country_choices=unique(selected_continent()$country)
    updateSelectInput(inputId="country", choices=country_choices)
  })
  
  output$data <- renderTable({
    req(input$country)
    selected_continent() %>% filter(country==input$country)
  })
  
}
shinyApp(ui, server)



#5. What is at the heart of the problem described at https://community.rstudio.com/t/29307?
#A: Mutually Dependent input values




####################################### Section 10.2 #######################################
#1. Use a hidden tabset to show additional controls only if the user checks an “advanced” check box.
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("advanced",label="Show Advanced Controls?")
    ),
  mainPanel(
    tabsetPanel(
      id="switcher",
      type="hidden",
      tabPanelBody("no_control","Check 'advanced' to show additional controls"),
      tabPanelBody("additional_control","Uncheck 'advanced' to hide additional controls")
    )
  )
 )
)
server <- function(input, output, session){
  observeEvent(input$advanced,{
    if(input$advanced==TRUE){
      updateTabsetPanel(inputId="switcher", selected="additional_control")
    }else{
      updateTabsetPanel(inputId="switcher", selected="no_control")
    }
  })
  
}
shinyApp(ui, server)


#2. Create an app that plots ggplot(diamonds, aes(carat)) but allows the user to choose which geom to use:
#geom_histogram(), geom_freqpoly(), or geom_density(). Use a hidden tabset to allow the user to select different
#arguments depending on the geom: geom_histogram() and geom_freqpoly() have a binwidth argument; geom_density() has a bw argument.




#3. Modify the app you created in the previous exercise to allow the user to choose whether each geom is
#shown or not (i.e. instead of always using one geom, they can picked 0, 1, 2, or 3).
#Make sure that you can control the binwidth of the histogram and frequency polygon independently.




####################################### Section 10.3 #######################################




