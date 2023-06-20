### Chapter 17 Notes ###

#library
library(shiny)
library(reactlog)


####################################### Notes #######################################
#This chapter introduces the most important software engineering skills you’ll need when writing Shiny apps:
#code organisation, testing, dependency management, source code control, continuous integration, and code reviews.

#Functions, the topic of Chapter 18, allow you to reduce duplication in your UI code, make your server functions easier to
#understand and test, and allow you to more flexibly organise your app code.

#Shiny modules, the topic of Chapter 19, make it possible to write isolated, re-usable code,
#that coordinates front end and back end behaviour. Modules allow you to gracefully separate concerns so
#that (e.g.) individual pages in your application can operate independently, or repeated components no
#longer need to be copied and pasted.

#An app’s dependencies are anything beyond the source code that it requires to run.

#For any analysis that you may want to reproduce in the future, consider using renv which enables you to create reproducible
#R environments. Using renv, you can capture the exact package versions that your application uses so that when you
#go to use this application on another computer, you can use exactly the same package versions. This is vital for
#apps run in production, not just because it gets the versions right on the first run, but because it also isolates
#your app from version changes over time.

#You can also rely on a “version-control system” that makes it easy to track atomic changes, roll back to previous work,
#and integrate the work of multiple contributors, with things like Git and GitHub.

#Typically, a code review involves someone other than you, but you can still benefit even if it’s only you.
#Most experienced developers will agree that taking a moment to review your own code often reveals some small flaw,
#particularly if you can let it sit for at least a few hours between writing and review.
#Here are few questions to hold in your head when reviewing code:
#- Do new functions have concise but evocative names?
#- Are there parts of the code you find confusing?
#- What areas are likely to change in the future, and would particularly benefit from automated testing?
#- Does the style of the code match the rest of the app? (Or even better, your group’s documented code style.)

