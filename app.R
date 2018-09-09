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
        sliderInput("numLineups", "Num of Lineups:", 1, 5, 3, step = 1, round = TRUE)
      ),
      
      # Panel: Stacking
      wellPanel(
        # Input: Stack Sizes
        sliderInput("stackSize1", "Stack Size 1:", 1, 5, 1, step = 1, round = TRUE),
        sliderInput("stackSize2", "Stack Size 2:", 1, 5, 1, step = 1, round = TRUE)
      ),
      
      # Panel: Exposure Summary
      wellPanel(
        # Output: Player Exposure
        h4("Exposure"),
        DT::dataTableOutput("exposureTable")
      )
    ),
    
    mainPanel(
      # Player Pool Output
      h3("Player Pool"),
      DT::dataTableOutput("poolTable"),
      # Lineups Output
      h3("Lineups"),
      DT::dataTableOutput("lineupsTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactives
  results <- reactive({
    req(input$numLineups)
    optimize_generic(pool, model, L = input$numLineups, 
                     stack_sizes = c(input$stackSize1, input$stackSize2))
  })
  
  lineups <- reactive({
    r <- results()
    df <- dplyr::bind_rows(r, .id = "lineup") %>% 
      select(lineup, player_id, player, team, opp_team, position, salary, fpts_proj)
    
    # order lineups by position
    pos2 <- factor(df[["position"]], levels = c("QB", "RB", "WR", "TE", "DST"))
    new_order <- order(df[["lineup"]], pos2, -df[["salary"]], df[["player"]]) 
    df[new_order,]
  })
  
  lineup_size <- reactive({
    r <- results()
    nrow(r[[1]])
  })
  
  
  # Player Pool Output
  output$poolTable <- DT::renderDataTable({
    pool_slim <- pool[c("player_id", "player", "team", "opp_team", "position", "salary", "fpts_proj")]
    pool_slim <- pool_slim[order(-pool_slim[["fpts_proj"]]),]
    
    DT::datatable(pool_slim, options = list(pageLength = 5)) %>% 
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
}

# Run the application 
shinyApp(ui = ui, server = server)

