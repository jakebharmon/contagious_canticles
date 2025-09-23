# # Load packages
pacman::p_load(
  DBI, RSQLite, tidyverse, survival, survminer, shiny, lubridate
)

survival_df <- readRDS("survival_data.Rds")

### Shiny app

explanation_text = "This app allows you to compare songs from the 1991 Sacred Harp songbook by their
\"contagiousness\" by adopting a tool from epidemiology called survival analysis. These charts
show people who lead a song within a year of first hearing it, and the curve shows how quickly people
come to lead a particular song. The faster a curve drops, the faster people lead that particular song.
This excludes people who never lead the song, or who don't lead it within a year of first hearing it."

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Contagious Canticles from the Sacred Harp"),
  
  # Sidebar layout with a dropdown for song selection
  sidebarLayout(
    sidebarPanel(
      # Slider for date range, moved to the sidebar for better organization
      sliderInput(
        inputId = "year_range",
        label = "Filter by Year First Sung:",
        min = min(year(survival_df$date_first_exposure)),
        max = max(year(survival_df$date_first_exposure)),
        value = c(min(year(survival_df$date_first_exposure)), max(year(survival_df$date_first_exposure))),
        step = 1,
        sep = ""
      ),
      helpText(explanation_text),
      br(),
      selectizeInput(
        inputId = "searchme",
        label = "Select one or more songs:",
        multiple = TRUE,
        # Choices will be populated by the server
        choices = NULL, 
        options = list(
          create = FALSE,
          placeholder = "Search for a song..."
        )
      )
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      plotOutput("survival_plot")
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive expression to filter the data based on user-selected year range
  filtered_by_year_df = reactive({
    survival_df %>%
      filter(year(date_first_exposure) >= input$year_range[1] & 
               year(date_first_exposure) <= input$year_range[2])
  })
  
  # Reactive expression for updating song choices based on the year slider
  observeEvent(filtered_by_year_df(), {
    
    # Get the currently selected songs
    current_selections = input$searchme
    
    song_choices_df = filtered_by_year_df() %>%
      distinct(song_id, Title, PageNum) %>%
      mutate(song_title = paste0(PageNum, " - ", Title)) %>%
      arrange(song_id)
    
    song_choices = setNames(song_choices_df$song_id, song_choices_df$song_title)
    
    # Filter the current selections to only include songs that are in the new choices
    filtered_selections = intersect(current_selections, song_choices_df$song_id)
    
    updateSelectizeInput(
      session,
      "searchme", 
      choices = song_choices,
      selected = filtered_selections # Pass the filtered selections here
    )
  })
  
  # Reactive expression to filter the data based on user selection
  filtered_data = reactive({
    req(input$searchme)
    # Filter the data that's already been filtered by year
    df_filtered <- filtered_by_year_df() %>%
      filter(song_id %in% input$searchme) %>% 
      # Create the 'song' column for the plot's strata
      mutate(song = paste0(" ", PageNum, " - ", Title),
             song = factor(song))
    
    return(df_filtered)
  })
  
  # Generate and render the ggsurvplot
  output$survival_plot <- renderPlot({
    
    # Get the filtered data
    df = filtered_data()
    
    fit = surv_fit(Surv(time_to_event_days, event_status) ~ song, data = df)
    
    p = ggsurvplot(
      fit,
      data = df,
      conf.int = FALSE,
      risk.table = TRUE,
      risk.table.col = "strata",
      ggtheme = theme_bw(),
      title = "Survival Curve by Song"
    ) 
    
    # Customize axis labels
    p$plot <- p$plot + 
      labs(
        y = "Proportion Who Haven't Led the Song Yet", 
        x = "Days Since First Hearing the Song"
      )
    
    print(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

#rsconnect::deployApp(appPrimaryDoc = "sacred_harp_km_app.R",appFiles = c("sacred_harp_km_app.R", "survival_data.Rds"))