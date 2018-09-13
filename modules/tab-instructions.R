library(shiny)

tabInstructionsUI <- function(id) {
  ns <- NS(id)
  
  YOUTUBE <- function(video_id) {
    iframe <- '<iframe width="560" height="315" src="https://www.youtube.com/embed/%s?rel=0&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'
    HTML(sprintf(iframe, video_id))
  }
  
  tabPanel(
    "INSTRUCTIONS",
    verticalLayout(
      h3("Importing Data"),
      YOUTUBE("_KMOlKHtwNU"),
      h3("Optimizing"),
      YOUTUBE("ND0tVJcDQr8"),
      h3("Exporting Lineups"),
      YOUTUBE("C-1DAYXwx-s")
    )
  )
}
