### Chapter 14 Notes & Exercise ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#Recall that reactive inputs and expressions are collectively called reactive producers; reactive expressions and outputs are reactive consumers.
#Invalidated: started state, yet to be run stage
#When an input changes, this kicks off an invalidation phase, which has three parts:
#invalidating the input, notifying the dependencies, then removing the existing connections.

#The reactlog package generates the so called reactlog which shows how the reactive graph evolves over time.
#To see the reactlog, you’ll need to first install the reactlog package, turn it on with reactlog::reactlog_enable(),
#then start your app. You then have two options:
#1. While the app is running, press Cmd + F3 (Ctrl + F3 on Windows), to show the reactlog generated up to that point.
#2. After the app has closed, run shiny::reactlogShow() to see the log for the complete session.
#Note: reactlog draws every dependency, even if it’s not currently used, in order to keep the automated layout stable.
#Connections that are not currently active (but were in the past or will be in the future) are drawn as thin dotted lines.

