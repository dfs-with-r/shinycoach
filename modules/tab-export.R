library(shiny)

tabExportUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Export Lineups",
    sidebarLayout(
      sidebarPanel(
        # Panel: Download Lineups
        wellPanel(
          h4("4. Download Lineups"),
          p("Download the lineups to a file ready to be uploaded to the DFS site."),
          downloadButton(ns("downloadLineups")) 
        )
      ),
      mainPanel(
        DT::dataTableOutput(ns("exportableTable"))
      )
    )
  )
}

tabExport <- function(input, output, session, lineups, 
                      siteChoices, sportChoices) {
  
  site <- reactive(tolower(siteChoices()))
  sport <- reactive(tolower(sportChoices()))
  
  split_lineups <- reactive({
    x <- lineups()
    split(x, x$lineup)
  })
  
  # Exportable Lineups Output
  output$exportableTable <- DT::renderDataTable({
    to_export <- coach::write_lineups(split_lineups(), site = site(), sport = sport())
    DT::datatable(
      to_export, 
      options = list(pageLength = 20, lengthChange = FALSE, searching = FALSE),
      rownames = FALSE
    )
  })
  
  # download lineups
  output$downloadLineups <- downloadHandler(
    filename = function() {
      now <- format(Sys.time(), "%Y%m%d-%H%M%S")
      sprintf("coach-%s-%s-%s.csv", site(), sport(), now)
    },
    content = function(file) {
      coach::write_lineups(split_lineups(), path = file, site = site(), sport = sport())
    },
    contentType = "text/csv"
  )
  
  # no reactives to return
  return(NULL)
}