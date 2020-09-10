library(shiny)

tabImportUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Import Data",
    sidebarLayout(
      sidebarPanel(
        
        # Panel: Define Model Type
        wellPanel(
          h4("1. Select Model"),
          p(" Select the type of optimization model to build. This defines the
              available positions, salary cap, and any other constraints."),
          radioButtons(ns("siteChoices"), "Site:", c("Fanduel", "Draftkings")),
          radioButtons(ns("sportChoices"), "Sport:", c("NFL", "NBA", "MLB", "NHL", "NASCAR", "PGA"))
        ),
        
        # Panel: Upload Site Player Pool
        wellPanel(
          h4("2. Upload Player Pool"),
          p("Export a player pool to your computer and upload it here:"),
          fileInput(ns("poolFilePicker"), NULL, multiple = FALSE)
        )
      ),
      mainPanel(
        # Player Pool Output
        h3("Player Pool"),
        DT::dataTableOutput(ns("poolTable"))
      )
    )
  )
}

tabImport <- function(input, output, session) {
  # choose file reader
  file_reader <- reactive({
    site <- input$siteChoices
    
    if (site == "Fanduel") coach::read_fd
    else if (site == "Draftkings") coach::read_dk
  })
  
  # player pool
  pool <- reactive({
    req(input$poolFilePicker, file_reader)
    
    # Get file
    file_meta <- input$poolFilePicker
    
    # Get reader
    reader <- file_reader()
    
    # Read data
    df <- reader(file_meta$datapath)
    df
  })
  
  # choose model
  model_maker <- reactive({
    req(input$poolFilePicker)
    
    sport <- input$sportChoices
    site <- input$siteChoices
    
    if (site == "Fanduel") {
      if (sport == "NFL") coach::model_fd_nfl
      else if (sport == "MLB") coach::model_fd_mlb
      else if (sport == "NBA") coach::model_fd_nba
      else if (sport == "NHL") coach::model_fd_nhl
      else if (sport == "NASCAR") coach::model_fd_nascar
      else if (sport == "PGA") coach::model_fd_pga
      else NULL
    } else if (site == "Draftkings") {
      if (sport == "NFL") coach::model_dk_nfl
      else if (sport == "MLB") coach::model_dk_mlb
      else if (sport == "NBA") coach::model_dk_nba
      else if (sport == "NHL") coach::model_dk_nhl
      else if (sport == "NASCAR") coach::model_dk_nascar
      else if (sport == "PGA") coach::model_dk_pga
      else NULL
    }
  })
  
  # build model
  model <- reactive({
    req(pool, model_maker)

    p <- pool()
    m <- model_maker()
    m(p)
  })
  
  # Player Pool Output
  output$poolTable <- DT::renderDataTable({
    p <- pool()
    pool_slim <- p[c("player_id", "player", "team", "opp_team", "position", "salary", "fpts_proj")]
    pool_slim <- pool_slim[order(-pool_slim[["fpts_proj"]]),]
    
    DT::datatable(pool_slim, options = list(pageLength = 15)) %>% 
      DT::formatRound("fpts_proj", 2)
  })
  
  # return a list of reactives
  list(
    "siteChoices" = reactive(input$siteChoices),
    "sportChoices" = reactive(input$sportChoices),
    "pool" = pool,
    "model" = model
  )
}
