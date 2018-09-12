library(shiny)

titleBarUI <- function(id, apptitle, github, email) {
  ns <- NS(id)

  # window title
  windowTitle <- tags$head(tags$title(apptitle))
  
  # github and mail url
  github_url <- paste("https://www.github.com/", github, sep = "/")
  email_url <- paste("mailto", email, sep = ":")
  
  # title bar
  titleRow <- fluidRow(
    column(9, h1(apptitle)),
    column(3, 
           a(icon("github", "fa-2x"), href = github_url, target = "_blank"),
           a(icon("envelope", "fa-2x"), href = email_url),
           style = "height:5.3em;padding-top:2em;padding-right:2em;text-align:right")
  )
  
  tagList(
    windowTitle,
    titleRow
  )
}
