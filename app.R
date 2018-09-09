library(shiny)
library(readr)
library(dplyr)
library(coach)

# Read data
pool <- coach::read_dk_nfl("../coach/tests/testthat/data/dk-nfl.csv")

# Add random projections
pool$fpts_proj <- rnorm(nrow(pool), pool$fpts_avg, 4)

# Build Model
model <- coach::model_dk_nfl(pool)

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Coach | DFS Lineup Optimizer"),
  
  # Sidebar layout with sidebar and main panels
  sidebarLayout(
    sidebarPanel(
      # Panel: Number of lineups
      wellPanel(
        # Input: Number of lineups
        sliderInput("numLineups", "Num of Lineups:", 1, 10, 3, step = 1, round = TRUE)
      ),
      
      # Panel: Stacking
      wellPanel(
        # Input: Stack Sizes
        sliderInput("stackSize1", "Stack Size 1:", 1, 5, 3, step = 1, round = TRUE),
        sliderInput("stackSize2", "Stack Size 2:", 1, 5, 2, step = 1, round = TRUE)
      )
    ),
    
    mainPanel(
      # Player Pool Output
      DT::dataTableOutput("poolTable"),
      # Lineups Output
      DT::dataTableOutput("lineupsTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Player Pool Output
  output$poolTable <- DT::renderDataTable({
    pool_slim <- pool[c("player_id", "player", "team", "opp_team", "position", "salary", "fpts_proj")]
    pool_slim <- pool_slim[order(-pool_slim[["fpts_proj"]]),]
    
    DT::datatable(pool_slim, options = list(pageLength = 5)) %>% 
      DT::formatRound("fpts_proj", 2)
  })
  
  # Lineups Output
  output$lineupsTable <- DT::renderDataTable({
    results <- optimize_generic(pool, model, L = input$numLineups)
    
    # combine results
    n <- nrow(results[[1]])
    lineups <- dplyr::bind_rows(results, .id = "lineup") %>% 
      select(lineup, player_id, player, team, opp_team, position, salary, fpts_proj)
    
    # order lineups by position
    pos2 <- factor(lineups[["position"]], levels = c("QB", "RB", "WR", "TE", "DST"))
    lineups <- lineups[order(lineups[["lineup"]], pos2, -lineups[["salary"]], lineups[["player"]]),]
    
    # render lineups
    DT::datatable(lineups, 
                  options = list(pageLength = n, lengthChange = FALSE, searching = FALSE),
                  rownames = FALSE) %>% 
      DT::formatRound("fpts_proj", 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

