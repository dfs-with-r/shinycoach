library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(coach)

# Define UI for application 
ui <- fluidPage(
  # Theme
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title
  titlePanel("Coach | DFS Lineup Optimizer"),
  
  # Sidebar layout with sidebar and main panels
  tabsetPanel(
    tabPanel(
      "Import Data",
      sidebarLayout(
        sidebarPanel(
          
          # Panel: Define Model Type
          wellPanel(
            h4("1. Select Model"),
            p(" Select the type of optimization model to build. This defines the
              available positions, salary cap, and any other constraints."),
            radioButtons("siteChoices", "Site:", c("Fanduel", "Draftkings")),
            radioButtons("sportChoices", "Sport:", c("NFL", "NBA", "MLB", "NHL"))
          ),
          
          # Panel: Upload Site Player Pool
          wellPanel(
            h4("2. Upload Player Pool"),
            p("Export a player pool to your computer and upload it here:"),
            fileInput("poolFilePicker", NULL, multiple = FALSE)
          )
        ),
        mainPanel(
          # Player Pool Output
          h3("Player Pool"),
          DT::dataTableOutput("poolTable")
        )
      )
    ),
    tabPanel(
      "Optimize",
      sidebarLayout(
        sidebarPanel(
          
          h4("3. Optimize Lineups"),
          p("Choose parameters to build the lineups that optimize total 
            projected fantasy points while meeting the site and sport constraints."),
          
          # Panel: Number of lineups
          wellPanel(
            # Input: Number of lineups
            sliderInput("numLineups", "Num of Lineups:", 1, 5, 3, step = 1, round = TRUE)
          ),
          
          # Panel: Stacking
          wellPanel(
            # Input: Stack Sizes
            sliderInput("stackSize1", "Stack Size 1:", 1, 4, 1, step = 1, round = TRUE),
            sliderInput("stackSize2", "Stack Size 2:", 1, 4, 1, step = 1, round = TRUE)
          )
        ),
        
        mainPanel(
          # Lineups Output
          h3("Lineups"),
          DT::dataTableOutput("lineupsTable"),
          # Output: Player Exposure
          h3("Exposure"),
          DT::dataTableOutput("exposureTable", width = "50%")
        )
      )
    ),
    
    tabPanel(
      "Export Lineups",
      sidebarLayout(
        sidebarPanel(
          # Panel: Download Lineups
          wellPanel(
            h4("4. Download Lineups"),
            p("Download the lineups to a file ready to be uploaded to the DFS site."),
            downloadButton("downloadLineups") 
          )
        ),
        mainPanel(
          DT::dataTableOutput("exportableTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # choose file reader
  file_reader <- reactive({
    cat("file_reader\n")
    site <- input$siteChoices
    
    if (site == "Fanduel") coach::read_fd
    else if (site == "Draftkings") coach::read_dk
  })
  
  # player pool
  pool <- reactive({
    req(input$poolFilePicker, file_reader)
    cat("pool\n")
    
    # Get file
    file_meta <- input$poolFilePicker
    
    # Get reader
    reader <- file_reader()
    
    # Read data
    df <- reader(file_meta$datapath)
    
    # Add random projections
    df$fpts_proj <- rnorm(nrow(df), df$fpts_avg, 4)
    df
  })
  
  # choose model
  model_maker <- reactive({
    req(input$poolFilePicker)
    cat("model_maker\n")
    sport <- input$sportChoices
    site <- input$siteChoices
    
    if (site == "Fanduel") {
      if (sport == "NFL") coach::model_fd_nfl
      else if (sport == "MLB") coach::model_fd_mlb
      else if (sport == "NBA") coach::model_fd_nba
      else if (sport == "NHL") coach::model_fd_nhl
      else NULL
    } else if (site == "Draftkings") {
      if (sport == "NFL") coach::model_dk_nfl
      else if (sport == "MLB") coach::model_dk_mlb
      else if (sport == "NBA") coach::model_dk_nba
      else NULL
    }
  })
  
  # build model
  model <- reactive({
    req(pool, model_maker)
    cat("model\n")

    p <- pool()
    m <- model_maker()
    m(p)
  })
  
  # optimization results
  results <- reactive({
    req(input$numLineups, pool, model)
    cat("results\n")
    optimize_generic(pool(), model(), L = input$numLineups, 
                     stack_sizes = c(input$stackSize1, input$stackSize2))
  })
  
  # combined lineups
  lineups <- reactive({
    cat("lineups\n")
    r <- results()
    
    # normalize lineups
    r <- lapply(r, coach::normalize_lineup, site = tolower(input$siteChoices), sport = tolower(input$sportChoices))
    
    # combine lineups
    df <- dplyr::bind_rows(r, .id = "lineup") %>% 
      select(lineup, player_id, player, team, opp_team, position, salary, fpts_proj)
    
    df
  })
  
  lineup_size <- reactive({
    cat("lineup_size\n")
    r <- results()
    nrow(r[[1]])
  })
  
  # Player Pool Output
  output$poolTable <- DT::renderDataTable({
    p <- pool()
    pool_slim <- p[c("player_id", "player", "team", "opp_team", "position", "salary", "fpts_proj")]
    pool_slim <- pool_slim[order(-pool_slim[["fpts_proj"]]),]
    
    DT::datatable(pool_slim, options = list(pageLength = 15)) %>% 
      DT::formatRound("fpts_proj", 2)
  })
  
  # Lineups Output
  output$lineupsTable <- DT::renderDataTable({
    DT::datatable(
      lineups(), 
      options = list(pageLength = lineup_size(), lengthChange = FALSE, searching = FALSE),
      rownames = FALSE
      ) %>% 
      DT::formatRound("fpts_proj", 2)
  })
  
  # Exposure Output
  output$exposureTable <- DT::renderDataTable({
    tbl <- lineups()
    nlineups <- tbl %>% distinct(lineup) %>% nrow()
    
    exposure <- tbl %>% 
      count(player_id, player, team, position) %>% 
      mutate(own = n/nlineups) %>% 
      select(player, team, own) %>% 
      arrange(desc(own), team, player)
    
    DT::datatable(
      exposure,
      options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE),
      rownames = FALSE
      ) %>% 
      DT::formatPercentage("own", 0)
  })
  
  # Exportable Lineups Output
  output$exportableTable <- DT::renderDataTable({
    to_export <- coach::write_lineups(lineups())
    DT::datatable(
      to_export, 
      options = list(pageLength = 20, lengthChange = FALSE, searching = FALSE),
      rownames = FALSE
    )
  })
  
  # download lineups
  output$downloadLineups <- downloadHandler(
    filename = function() {
      site <- tolower(input$siteChoices)
      sport <- tolower(input$sportChoices)
      now <- format(Sys.time(), "%Y%m%d-%H%M%S")
      sprintf("coach-%s-%s-%s.csv", site, sport, now)
      },
    content = function(file) {
      cat(file, "\n")
      #file.copy("data/coach-template.csv", file)
      coach::write_lineups(lineups(), file)
    },
    contentType = "text/csv"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

