### Chapter 13 Notes & Exercise ###

#library
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


####################################### Notes #######################################
#Reactive programming is a style of programming that focuses on values that change over time,
#and calculations and actions that depend on those values. Reactivity is important for Shiny apps
#because they’re interactive: users change input controls (dragging sliders, typing in textboxes, checking checkboxes,…)
#which causes logic to run on the server (reading CSVs, subsetting data, fitting models,…) ultimately resulting in outputs
#updating (plots redrawing, tables updating, …). This is quite different from most R code, which typically
#deals with fairly static data.

#For Shiny apps to be maximally useful, we need reactive expressions and outputs to update if and only if their
#inputs change. We want outputs to stay in sync with inputs, while ensuring that we never do more work than necessary.

#Event-driven programming is an appealingly simple paradigm: you register callback functions that will be executed
#in response to events.
#We could implement a very simple event-driven toolkit using R6, as in the example below.
#Here we define a DynamicValue that has three important methods: get() and set() to access and change
#the underlying value, and onUpdate() to register code to run whenever the value is modified.
#If you’re not familiar with R6, don’t worry about the details, and instead focus on following examples.
DynamicValue <- R6::R6Class("DynamicValue", list(
  value = NULL,
  on_update = NULL,
  
  get = function() self$value,
  
  set = function(value) {
    self$value <- value
    if (!is.null(self$on_update)) 
      self$on_update(value)
    invisible(self)
  },
  
  onUpdate = function(on_update) {
    self$on_update <- on_update
    invisible(self)
  }
))

#Reactive programming solves the problem of unnecessary computation (like event-driven programming), but also
#solves the problem with event-driven programming: You have to carefully track which inputs affect which computations

#Now we can show you some real Shiny code, using a special Shiny mode, reactiveConsole(TRUE),
#that makes it possible to experiment with reactivity directly in the console.
library(shiny)
reactiveConsole(TRUE)
#As with event-driven programming, we need some way to indicate that we have a special type of variable.
#In Shiny, we create a reactive value with reactiveVal(). A reactive value has special syntax42 for getting
#its value (calling it like a zero-argument function) and setting its value (set its value by calling
#it like a one-argument function).
temp_c <- reactiveVal(10) # create
temp_c()                  # get
#> [1] 10
temp_c(20)                # set
temp_c()                  # get
#> [1] 20
#Now we can create a reactive expression that depends on this value:
temp_f <- reactive({
  message("Converting") 
  (temp_c() * 9 / 5) + 32
})
temp_f()
#> Converting
#> [1] 68
#As you’ve learned when creating apps, a reactive expression automatically tracks all of its dependencies.
#So that later, if temp_c changes, temp_f will automatically update:

#A reactive expression has two important properties:
#1.) It's lazy: it doesn't do any work until it's called.
#2.) It's cached: it doesn't do any work the second and subsequent times it's called because it caches the previous result.






