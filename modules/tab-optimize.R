library(shiny)

tabOptimizeUI <- function(id) {
  ns <- NS(id)
  
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
          sliderInput(ns("numLineups"), "Num of Lineups:", 1, 5, 3, step = 1, round = TRUE)
        ),
        
        # Panel: Stacking
        wellPanel(
          # Input: Stack Sizes
          sliderInput(ns("stackSize1"), "Stack Size 1:", 1, 4, 1, step = 1, round = TRUE),
          sliderInput(ns("stackSize2"), "Stack Size 2:", 1, 4, 1, step = 1, round = TRUE)
        )
        ),
      
      mainPanel(
        # Lineups Output
        h3("Lineups"),
        DT::dataTableOutput(ns("lineupsTable")),
        # Output: Player Exposure
        h3("Exposure"),
        DT::dataTableOutput(ns("exposureTable"), width = "50%")
      )
    )
  )
}

tabOptimize <- function(input, output, session, pool, model,
                        siteChoices, sportChoices) {
  # optimization results
  results <- reactive({
    req(input$numLineups, pool, model)
    
    # check pool before building model
    p <- pool()
    m <- model()

    validate(
      need(all(!is.na(p[["fpts_proj"]])), message = "fpts_proj can't be empty or contain NAs")
    )
    
    optimize_generic(p, m, L = input$numLineups, 
                     stack_sizes = c(input$stackSize1, input$stackSize2))
  })
  
  # combined lineups
  lineups <- reactive({
    r <- results()
    site <- tolower(siteChoices())
    sport <- tolower(sportChoices())
    
    # normalize lineups
    r <- lapply(r, coach::normalize_lineup, site = site, sport = sport)
    
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
  
  # return reactives
  list("lineups" = lineups)
}
