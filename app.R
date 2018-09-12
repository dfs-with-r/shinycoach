library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(coach)

source("module-title.R")
source("module-tab-import.R")

# Define UI for application 
ui <- fluidPage(
  # Theme
  theme = shinythemes::shinytheme("cosmo"),
  
  # Title Bar
  titleBarUI("title", "Coach | DFS Lineup Optimizer", 
             github = "dfs-with-r/coach", email = "robert@dfswithr.com"),

  # Sidebar layout with sidebar and main panels
  tabsetPanel(
    tabImportUI("import"),
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
  
  # import module
  import_reactives <- callModule(tabImport, "import")
  pool <- import_reactives[["pool"]]
  model <- import_reactives[["model"]]
  
  # optimization results
  results <- reactive({
    req(input$numLineups, pool, model)
    
    optimize_generic(pool(), model(), L = input$numLineups, 
                     stack_sizes = c(input$stackSize1, input$stackSize2))
  })
  
  # combined lineups
  lineups <- reactive({
    r <- results()
    
    # normalize lineups
    r <- lapply(r, coach::normalize_lineup, site = tolower(input$siteChoices), sport = tolower(input$sportChoices))
    
    # combine lineups
    df <- dplyr::bind_rows(r, .id = "lineup") %>% 
      select(lineup, player_id, player, team, opp_team, position, salary, fpts_proj)
    
    df
  })
  
  lineup_size <- reactive({
    r <- results()
    nrow(r[[1]])
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
      coach::write_lineups(lineups(), file)
    },
    contentType = "text/csv"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

