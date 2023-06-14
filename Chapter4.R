### Chapter 4 Case Study: ER Injuries ###

#library
library(shiny)
library(vroom)
library(tidyverse)
library(data.table)

#Data from the National Electronic Injury Surveillance System (NEISS), collected by the Consumer Product Safety Commission
#Focusing on just year 2017
dir.create("neiss")
#data manually downloaded from: https://github.com/hadley/mastering-shiny/tree/main/neiss
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- as.data.frame(fread("neiss/products.tsv"))
population <- as.data.frame(fread("neiss/population.tsv"))


#Data Exploration for interesting story: "toilets" (prod_code 649)
selected <- injuries %>% filter(prod_code == 649)
nrow(selected) #> [1] 2993
selected %>% count(location, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(diag, wt = weight, sort = TRUE)
summary <- selected %>% count(age, sex, wt = weight)
summary
#create plot of summary above
summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

#by injury rate, to control for difference in injured data among younger vs older people
summary <- selected %>% 
            count(age, sex, wt = weight) %>% 
            left_join(population, by = c("age", "sex")) %>% 
            mutate(rate = n / population * 1e4)
#create plot by injury rate, instead of counts
summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")


#function to create summary table, with limiting how many rows show up
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}




##### Creating Shiny App #####
prod_codes <- setNames(products$prod_code, products$title) #this shows the product name in the UI and returns the product code to the server

ui <- fluidPage(
  fluidRow( #one row for inputs
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    column(2, numericInput("nrows","How many rows?", min=1, max=10, value=5, width="100%"))
  ),
  fluidRow( #one row for tables
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow( #one row for plots
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, span("Narrative Display:", style = "font-weight: bold")),
    column(1, actionButton(inputId ="Previous", label = icon("arrow-left"))),
    column(1, actionButton(inputId ="Next", label = icon("arrow-right"))),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  #converting the selected and summary variables created in the previous section to reactive expressions
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  n_row <- reactive(input$nrows) # number of rows displayed in tables
  
  output$diag <- renderTable(count_top(selected(), diag, n=n_row()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n=n_row()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n=n_row()), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  
  ### narrative
  num_narr <- reactive(
    length(selected()$narrative)
  )
  
  # a reactive value that can be easily changed later (in events)
  # ref: https://stackoverflow.com/questions/42183161/r-shiny-how-to-change-values-in-a-reactivevalues-object
  i <- reactiveValues(tmp=1)
  
  # reset i to 1 if code is changed by user
  # ref: https://www.collinberke.com/post/shiny-series-implementing-a-next-and-back-button/
  observeEvent(input$code, {
    i$tmp <- 1
  })
  
  output$narrative <- renderText({
    selected()$narrative[1]
  })
  
  observeEvent(input$Next, {
    
    i$tmp <- i$tmp + 1
    
    if(i$tmp <= num_narr()){
      output$narrative <- renderText({
        selected()$narrative[i$tmp]
      })
    } else{
      i$tmp <- 1
      output$narrative <- renderText({
        selected()$narrative[1]
      })
    }
  })
  
  observeEvent(input$Previous, {
    i$tmp <- i$tmp - 1
    
    if(i$tmp > 0){
      output$narrative <- renderText({
        selected()$narrative[i$tmp]
      })
    } else{
      i$tmp <- num_narr()
      output$narrative <- renderText({
        selected()$narrative[num_narr()]
      })
    }
  })
  
}

shinyApp(ui, server)
