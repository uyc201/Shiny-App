### Chapter 9 Notes & Exercise ###

#library
library(shiny)


####################################### Notes #######################################
#Upload
#The UI needed to support file uploads is simple: just add fileInput() to your UI:
ui <- fluidPage(
  fileInput("upload", "Upload a file")
)
#Most inputs return simple vectors, but fileInput() returns a data frame with four columns:
#name: the original file name on the user’s computer.
#size: the file size, in bytes.
#type: the “MIME type”31 of the file.
#datapath: the path to where the data has been uploaded on the server.
ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files")
)
server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
}
#If the user is uploading a dataset, there are two details that you need to be aware of:
#1. input$upload is initialized to NULL on page load, so you’ll need req(input$upload) to make sure your code waits until the first file is uploaded.
#2. The accept argument allows you to limit the possible inputs.

#See an example of uploading a .csv or .tsv file and see the first n rows, below:
#See it on action here: https://hadley.shinyapps.io/ms-upload-validate
ui <- fluidPage(
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}




#Download
#Again, the UI is straightforward: use either downloadButton(id) or downloadLink(id) to give the user something to click to download a file.
ui <- fluidPage(
  downloadButton("download1"),
  downloadLink("download2")
)

#Unlike other outputs, downloadButton() is not paired with a render function. Instead, you use downloadHandler():
output$download <- downloadHandler(
  filename = function() {
    paste0(input$dataset, ".csv")
  },
  content = function(file) {
    write.csv(data(), file)
  }
)


#The following app shows off the basics of data download by allowing you to download any dataset
#in the datasets package as a tab separated file:
ui <- fluidPage(
  selectInput("dataset", "Pick a dataset", ls("package:datasets")),
  tableOutput("preview"),
  downloadButton("download", "Download .tsv")
)

server <- function(input, output, session) {
  data <- reactive({
    out <- get(input$dataset, "package:datasets")
    if (!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame")) #Note the use of validate() to only allow the user to download datasets that are data frames
    }
    out
  })
  
  output$preview <- renderTable({
    head(data())
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
}



#As well as downloading data, you may want the users of your app to download a report that summarizes the result of interactive exploration in the Shiny app.
#One powerful way to generate such a report is with a parameterised RMarkdown document. A parameterised RMarkdown file has a params field in the YAML metadata:
title: My Document
output: html_document
params:
  year: 2018
region: Europe
printcode: TRUE
data: file.csv


#Here’s a simple example adapted from https://shiny.rstudio.com/articles/generating-reports.html, which describes this technique in more detail.
ui <- fluidPage(
  sliderInput("n", "Number of points", 1, 100, 50),
  downloadButton("report", "Generate report")
)

server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$n)
      
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render("report.Rmd", 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}




####################################### Case Study #######################################
#To finish up, we’ll work through a small case study where we upload a file (with user supplied separator),
#preview it, perform some optional transformations using the janitor package, by Sam Firke,
#and then let the user download it as a .tsv.

#uploading & parsing the file:
ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Raw data"),
    tableOutput("preview1")
  )
)
#cleaning the file:
ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake", "Rename columns to snake case?"),
    checkboxInput("constant", "Remove constant columns?"),
    checkboxInput("empty", "Remove empty cols?")
  ),
  mainPanel(
    h3("Cleaner data"),
    tableOutput("preview2")
  )
)
#downloading the file:
ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)
#assembled together to a single fluidPage():
ui <- fluidPage(
  ui_upload,
  ui_clean,
  ui_download
)

#then the server portion of the app:
server <- function(input, output, session) {
  # Upload ---------------------------------------------------------
  raw <- reactive({
    req(input$file)
    delim <- if (input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))
  
  # Clean ----------------------------------------------------------
  tidied <- reactive({
    out <- raw()
    if (input$snake) {
      names(out) <- janitor::make_clean_names(names(out))
    }
    if (input$empty) {
      out <- janitor::remove_empty(out, "cols")
    }
    if (input$constant) {
      out <- janitor::remove_constant(out)
    }
    
    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))
  
  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )
}




####################################### Section 9.4 #######################################
#1. Use the ambient package by Thomas Lin Pedersen to generate worley noise and download a PNG of it.
#install.packages('devtools')
devtools::install_github('thomasp85/ambient')

ui <- fluidPage(
  numericInput("n1","First Noise n",value=100, min=100, max=500),
  numericInput("n2","Second Noise n",value=100, min=100, max=500),
  plotOutput("noise_plot"),
  downloadButton("download_plot")
)

server <- function(input, output, session){
  noise <- reactive(ambient::noise_worley(c(as.numeric(input$n1), as.numeric(input$n2))))
  
  output$noise_plot <- renderPlot(plot(as.raster(ambient::normalise(noise()))))
  
  plotInput <- function(){
    plot(as.raster(ambient::normalise(noise())))
  }
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("Noise Plot.png")
    },
  content = function(file){
    png(plotInput(), file)
  }
  )
}

shinyApp(ui, server)



#2. Create an app that lets you upload a csv file, select a variable, and then perform a t.test() on that variable.
#After the user has uploaded the csv file, you’ll need to use updateSelectInput() to fill in the available variables.
#See Section 10.1 for details.



#3. Create an app that lets the user upload a csv file, select one variable, draw a histogram, and then download the histogram.
#For an additional challenge, allow the user to select from .png, .pdf, and .svg output formats.



#4. Write an app that allows the user to create a Lego mosaic from any .png file using Ryan Timpe’s brickr package.
#Once you’ve completed the basics, add controls to allow the user to select the size of the mosaic (in bricks),
#and choose whether to use “universal” or “generic” colour palettes.



#5. The final app in Section 9.3 contains this one large reactive. Break it up into multiple pieces so that (e.g.)
#janitor::make_clean_names() is not re-run when input$empty changes.
tidied <- reactive({
  out <- raw()
  if (input$snake) {
    names(out) <- janitor::make_clean_names(names(out))
  }
  if (input$empty) {
    out <- janitor::remove_empty(out, "cols")
  }
  if (input$constant) {
    out <- janitor::remove_constant(out)
  }
  
  out
})

