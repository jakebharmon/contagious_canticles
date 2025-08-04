# # Load packages
pacman::p_load(
  DBI, RSQLite, tidyverse, survival, survminer, plotly, shiny
)

# Connect to SQl Database
con = dbConnect(SQLite(), "minutes.db")

# Create table of all data tables
tables = as.data.frame(dbListTables(con))

# Query combing song data
sql_minutes = con %>% dbGetQuery(
  "SELECT leader_id, song_id, minutes.id, minutes.date, songs.PageNum, songs.Title
  FROM song_leader_joins
  INNER JOIN songs ON song_leader_joins.song_id = songs.id
  INNER JOIN minutes ON song_leader_joins.minutes_id = minutes.id
  INNER JOIN minutes_location_joins ON minutes_location_joins.minutes_id = minutes.id")

## To fix, multi-day singing events
minutes = sql_minutes %>% 
  mutate(date = mdy(Date)) %>% 
  select(-c(Date)) %>% 
  drop_na()

# This is the date the person first led a specific song
first_sung_df = minutes %>%
  group_by(leader_id, song_id,Title, PageNum) %>%
  summarise(
    date_first_sung = min(date),
    .groups = 'drop'
  )

# A person is "exposed" to a song on any date they were a leader at a meeting 
# where that song was sung.

# First, get a list of all leaders who attended each meeting date
attendance_df = minutes %>%
  distinct(date, id, leader_id)

# Next, get a list of all songs sung at each meeting date
songs_at_meeting_df = minutes %>%
  distinct(date, id, song_id)

# Now, we join these two tables to find every possible leader-song-date combo
exposure_df = attendance_df %>%
  inner_join(songs_at_meeting_df, by = "date") %>%
  # Now, for each unique leader-song pair, find the first date
  group_by(leader_id, song_id) %>%
  summarise(
    date_first_exposure = min(date),
    .groups = 'drop'
  )

# We use a full_join to keep all person-song combinations,
# including those who were exposed but never sang the song.
final_df = full_join(exposure_df, 
                     first_sung_df, 
                     by = c("leader_id", "song_id")) %>%
  mutate(
    # The date of first sung is not NA if the person actually sang the song
    event_status = as.integer(!is.na(date_first_sung)),
    # Calculate the time to event in days, only if the event occurred
    time_to_event_days = case_when(
      event_status == 1 ~ as.integer(date_first_sung - date_first_exposure),
      TRUE ~ NA_integer_
    )
  ) %>%
  # Filter out combos which where time to event is zero, which means that
  # a person sung a song at the first meeting which it was heard; likely
  # meaning they otherwise heard the song beforehand
  filter((time_to_event_days != 0 | is.na(time_to_event_days) == TRUE) & time_to_event_days <= 365) %>% 
  drop_na()

## For export and sheet analysis
survival_df = final_df %>%
  select(song_id, event_status, time_to_event_days, Title, PageNum) %>% 
  write_csv("survival_analysis.csv")


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
      plotlyOutput("survival_plot")
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
  output$survival_plot <- renderPlotly({
    
    # Get the filtered data
    df = filtered_data()
    
    fit = surv_fit(Surv(time_to_event_days, event_status) ~ song, data = df)
    
    p = ggsurvplot(
      fit,
      data = df,
      risk.table = TRUE,
      risk.table.col = "strata",
      ggtheme = theme_bw(),
      title = "Survival Curve by Song"
    ) 
    
    # Convert the ggsurvplot ggplot object to an interactive plotly object.
    # We pass p$plot, which is the main survival plot.
    ggplotly(p$plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
rsconnect::deployApp(
       appDir = "/Users/jake/Documents/sacred_harp_km/survival_analysis",
       appPrimaryDoc = "survival_analysis_sacred_harp_app.R"
   )