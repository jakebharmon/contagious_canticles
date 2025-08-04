# # Load packages
pacman::p_load(
  DBI, RSQLite, tidyverse, survival, survminer, plotly, shiny, readr
)

survival_df = readRDS("final_df.rds") %>%
 select(song_id, event_status, time_to_event_days, Title, PageNum)

### Shiny app

# Define UI for the application
ui <- fluidPage(
  
  
  # Application title
  titlePanel("Infectious Songs from the Sacred Harp"),
  
  # Sidebar layout with a dropdown for song selection
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "searchme",
        label = "Search Bar",
        multiple = TRUE,
        # Choices will be populated by the server
        choices = NULL, 
        options = list(
          create = FALSE,
          placeholder = "Search Me"
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
  # Get the unique names of the songs to populate the dropdown
  song_choices_df = survival_df %>%
    distinct(song_id, Title, PageNum) %>%
    mutate(song_title = paste0(PageNum, " - ", Title)) %>%
    arrange(song_id)
  
  # The values will be the song_id, names will be the song_title
  song_choices = setNames(song_choices_df$song_id, song_choices_df$song_title)
  
  # Update the choices for the dropdown list
  # This observe block will run once at startup to populate the choices.
  observe({
    updateSelectizeInput(
      session,
      "searchme", 
      choices = song_choices,
      selected = NULL # No default selection for a clean start
    )
  })
  
  # Reactive expression to filter the data based on user selection
  filtered_data = reactive({
    req(input$searchme)
    # Filter the original data to include only the selected songs
    df_filtered <- survival_df %>%
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
    
    ggsurvplot(
      fit,
      data = df,
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.col = "strata",
      ggtheme = theme_bw(),
      title = "Survival Curve by Song",
      palette = "viridis"
    ) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
