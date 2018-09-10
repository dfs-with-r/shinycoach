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
      "Import",
      sidebarLayout(
        sidebarPanel(
          
          # Panel: Upload Site Player Pool
          wellPanel(
            h4("1. Upload Player Pool"),
            radioButtons("siteChoices", "Site:", c("Fanduel", "Draftkings")),
            p("Export a player pool to your computer and upload it here:"),
            fileInput("poolFilePicker", NULL, multiple = FALSE)
          ),
          
          # Panel: Download Template
          wellPanel(
            h4("2. Download Template"),
            p("Download the template and fill in the `fpts_proj` column with your projections."),
            downloadButton("downloadTemplate") 
          ),
          
          # Panel: Upload Data
          wellPanel(
            h4("3. Upload Pool with Projections"),
            p("Upload the new file with the projections in them."),
            fileInput("projFilePicker", NULL, multiple = FALSE)
          ),
          
          # Panel: Select Model
          wellPanel(
            h4("4. Select Sport"),
            p(" Select the type of optimization model to build. This defines the
              available positions, salary cap, and any other constraints."),
            radioButtons("sportChoices", "Sport:", c("NFL", "NBA", "MLB", "NHL"))
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
    )
  )
  
)

# Define server logic
server <- function(input, output) {
  # Reactives
  file_reader <- reactive({
    cat("file_reader\n")
    site <- input$siteChoices
    
    if (site == "Fanduel") coach::read_fd
    else if (site == "Draftkings") coach::read_dk
  })
  
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
  
  model_maker <- reactive({
    #req(input$projFilePicker)
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
  
  model <- reactive({
    req(pool, model_maker)#, input$projFilePicker)
    cat("model\n")
    # Build Model
    p <- pool()
    m <- model_maker()
    m(p)
  })
  
  results <- reactive({
    req(input$numLineups, pool, model)
    cat("results\n")
    optimize_generic(pool(), model(), L = input$numLineups, 
                     stack_sizes = c(input$stackSize1, input$stackSize2))
  })
  
  pos_levels <- reactive({
    cat("pos_levels\n")
    switch(input$sportChoices,
           "NFL" = c("QB", "RB", "WR", "TE", "DST"),
           "MLB" = c("P", "C", "1B", "2B", "3B", "SS", "OF"),
           "NBA" = c("PG", "SG", "SF", "PF", "C"),
           "NHL" = c("G", "C", "W", "D")
           )
  })
  
  lineups <- reactive({
    cat("lineups\n")
    r <- results()
    df <- dplyr::bind_rows(r, .id = "lineup") %>% 
      select(lineup, player_id, player, team, opp_team, position, salary, fpts_proj)
    
    # order lineups by position
    pos2 <- factor(df[["position"]], levels = pos_levels())
    new_order <- order(df[["lineup"]], pos2, -df[["salary"]], df[["player"]]) 
    df[new_order,]
  })
  
  lineup_size <- reactive({
    cat("lineup_size\n")
    r <- results()
    nrow(r[[1]])
  })
  
  
  # Player Pool Output
  output$poolTable <- DT::renderDataTable({
    p <- pool()
    
    # check if fpts_proj exists
    if (!("fpts_proj" %in% colnames(p))) {
      p[["fpts_proj"]] <- NA_real_
    }
    
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
      select(player, team, position, own) %>% 
      arrange(desc(own), team, player)
    
    DT::datatable(
      exposure,
      options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE),
      rownames = FALSE
      ) %>% 
      DT::formatPercentage("own", 0)
  })
  
  # download template
  output$downloadTemplate <- downloadHandler(
    filename = function() {"coach-template.csv"},
    content = function(file) {
      cat(file, "\n")
      #file.copy("data/coach-template.csv", file)
      p <- pool()
      # check if fpts_proj exists
      if (!("fpts_proj" %in% colnames(p))) {
        p[["fpts_proj"]] <- NA_real_
      }
      
      pool_slim <- p[c("player_id", "player", "team", "opp_team", "position", "salary", "fpts_proj")]
      pool_slim <- pool_slim[order(-pool_slim[["fpts_proj"]]),]
      
      readr::write_csv(pool_slim, file)
    },
    contentType = "text/csv"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

