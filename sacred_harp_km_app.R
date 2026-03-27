pacman::p_load(
  tidyverse, survival, broom, scales, shiny, bslib, lubridate, DT, stringr
)

survival_df <- readRDS("survival_data.Rds")

removed_pages <- c(
  "41", "50t", "50b", "51", "54", "55", "80t", "81b", "115", "116",
  "118", "130", "140", "160t", "161", "169", "184", "188", "213b",
  "223", "231", "242", "254", "263", "266", "279", "293", "295",
  "308", "323b", "330t", "331", "334", "355", "367", "381", "393",
  "395", "409", "413", "415", "429", "433", "437", "438", "449",
  "451", "453", "459", "461", "463", "467", "468", "471", "476",
  "478", "483", "484", "487", "488", "491", "493", "494", "502",
  "513", "515", "516", "523", "524", "531", "539", "541", "544",
  "545", "562", "570", "571"
)

survival_df <- survival_df %>%
  mutate(removed_2025 = ifelse(PageNum %in% removed_pages, TRUE, FALSE))

default_songs <- c(35, 258, 311)

compute_median_survival <- function(df) {
  df %>%
    group_by(song_id, Title, PageNum) %>%
    summarise(
      median_days = median(time_to_event_days, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# ------ UI ------
ui <- page_navbar(
  title = "Contagious Canticles",
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Playfair Display")
  ),

  sidebar = sidebar(
    width = 300,

    selectizeInput(
      inputId = "searchme",
      label = "Select one or more songs:",
      multiple = TRUE,
      choices = NULL,
      options = list(
        create = FALSE,
        placeholder = "Search by page or title..."
      )
    ),

    sliderInput(
      inputId = "year_range",
      label = "Filter by year of first exposure:",
      min = min(year(survival_df$date_first_exposure)),
      max = max(year(survival_df$date_first_exposure)),
      value = c(
        min(year(survival_df$date_first_exposure)),
        max(year(survival_df$date_first_exposure))
      ),
      step = 1,
      sep = ""
    ),

    checkboxInput(
      inputId = "highlight_removed",
      label = "Mark songs removed in 2025 edition",
      value = FALSE
    ),

    hr(),

    h6("Quick picks"),
    actionButton("pick_most",  "Most contagious",  class = "btn-sm btn-outline-success w-100 mb-1"),
    actionButton("pick_least", "Least contagious", class = "btn-sm btn-outline-warning w-100 mb-1"),
    actionButton("pick_clear", "Clear",            class = "btn-sm btn-outline-secondary w-100")
  ),

  # ---- Tab 1: Survival Curves ----
  nav_panel(
    "Survival Curves",

    layout_columns(
      col_widths = 12,

      card(
        card_header(class = "bg-primary text-white",
                    h4("What if songs were contagious?", class = "mb-0")),
        card_body(
          p("Explore how Sacred Harp songs spread through the singing community,
            using survival analysis on the ",
            tags$a(href = "https://fasola.org", target = "_blank", "FaSoLa Minutes"),
            " database. Select songs to compare their \u2018contagiousness\u2019\u2014how
            quickly singers come to lead them after first hearing them."),
          tags$ul(
            class = "mb-0",
            tags$li("Each curve tracks how quickly people come to ",
                    tags$strong("lead"), " a song after first hearing it."),
            tags$li("The y-axis shows the proportion who ", tags$em("haven\u2019t"),
                    " led the song yet \u2014 so it always starts at 100%."),
            tags$li("A ", tags$strong("faster drop"), " means the song spreads more
                    quickly through the community."),
            tags$li("Curves are censored at one year (365 days). People who never
                    led the song are counted but their timeline ends there.")
          )
        )
      ),

      card(
        card_header("Survival Curves"),
        card_body(
          plotOutput("survival_plot", height = "700px")
        )
      ),

      card(
        card_header("Selected Songs \u2014 Ranked by Median Days to Lead"),
        card_body(
          DTOutput("leaderboard_table")
        )
      )
    )
  ),

  # ---- Tab 2: Full Data Table ----
  nav_panel(
    "All Song Data",

    card(
      card_header("Complete Exposure Records"),
      card_body(
        tags$p(class = "text-muted small mb-3",
               "One row per song, across all years. \u2018Median Days to Lead\u2019 is the median
               time from first hearing to first leading. \u2018% Who Led\u2019 is the share of
               exposed singers who led the song within the observation window."),
        DTOutput("full_table")
      )
    )
  ),

  # ---- Tab 3: Methodology ----
  nav_panel(
    "Methodology",

    card(
      card_body(
        class = "px-4 py-4",
        style = "max-width: 800px; margin: 0 auto;",

        h3("About This Project"),
        p("This app applies ", tags$strong("Kaplan-Meier survival analysis"),
          "\u2014a technique borrowed from epidemiology\u2014to the ",
          tags$a(href = "https://fasola.org", target = "_blank", "FaSoLa Minutes"),
          " database, which records the leaders and attendees of Sacred Harp singings
          going back to the 1980s."),

        h4("The core question"),
        p("When a singer first hears a song, how long does it take before they lead
          it themselves? Do some songs spread faster than others? And if so, what
          makes a song \u2018contagious\u2019?"),

        h4("How the data works"),
        tags$ul(
          tags$li(tags$strong("Exposure:"), " A person is considered \u2018exposed\u2019 to a song
                  the first time they attend a singing where that song was led by anyone.
                  Because the minutes record only who led each song, exposure is inferred:
                  if you led any song at a singing, you are assumed to have been present
                  for all songs led that day."),
          tags$li(tags$strong("Event:"), " The \u2018event\u2019 is the first time that person
                  leads the song themselves."),
          tags$li(tags$strong("Censoring:"), " If a person heard a song but never led it
                  within the observation window (or within one year of first exposure),
                  they are counted as \u2018censored\u2019\u2014their timeline ends without an event.
                  Censored observations are included in the analysis; they just contribute
                  information up until the point they exit the risk set.")
        ),

        h4("Why Kaplan-Meier curves?"),
        p("Kaplan-Meier curves handle censoring correctly. A simple average of \u2018days
          to lead\u2019 would be biased because it ignores everyone who never led the song.
          The KM estimator accounts for them, giving an unbiased picture of the true
          distribution of time-to-lead across the whole community."),
        p("The ", tags$strong("median survival time"), " shown in the leaderboard table
          is the point at which 50% of exposed singers have led the song. A lower number
          means the song spreads more quickly."),

        h4("Limitations"),
        tags$ul(
          tags$li("Exposure is inferred, not directly observed. A singer could have
                  been absent from part of a singing."),
          tags$li("The minutes database is not complete for all singings, particularly
                  in earlier decades and outside the American South."),
          tags$li("Songs with fewer than ~30 exposures should be interpreted cautiously;
                  small samples make the curves unstable."),
          tags$li("The 2025 edition of ", tags$em("The Sacred Harp"), " removed a number
                  of songs. Songs marked \u2020 in the selector were removed in that revision.")
        ),

        h4("Data source"),
        p("All data comes from the ",
          tags$a(href = "https://fasola.org", target = "_blank", "FaSoLa Minutes"),
          " database, maintained by the Sacred Harp Musical Heritage Association.")
      )
    )
  )
)

# ------ Server ------
server <- function(input, output, session) {

  filtered_by_year_df <- reactive({
    survival_df %>%
      filter(
        year(date_first_exposure) >= input$year_range[1],
        year(date_first_exposure) <= input$year_range[2]
      )
  })

  observeEvent(filtered_by_year_df(), {
    current_selections <- input$searchme

    song_choices_df <- filtered_by_year_df() %>%
      distinct(song_id, Title, PageNum, removed_2025) %>%
      mutate(
        label = paste0(PageNum, " \u2013 ", Title,
                       ifelse(removed_2025, " \u2020", ""))
      ) %>%
      arrange(song_id)

    song_choices <- setNames(song_choices_df$song_id, song_choices_df$label)
    filtered_selections <- intersect(current_selections, song_choices_df$song_id)

    if (is.null(current_selections)) {
      filtered_selections <- intersect(default_songs, song_choices_df$song_id)
    }

    updateSelectizeInput(session, "searchme",
                         choices = song_choices,
                         selected = filtered_selections)
  })

  song_medians <- compute_median_survival(survival_df)

  observeEvent(input$pick_most, {
    top5 <- song_medians %>%
      filter(n >= 30) %>% arrange(median_days) %>% head(5) %>% pull(song_id)
    updateSelectizeInput(session, "searchme", selected = top5)
  })

  observeEvent(input$pick_least, {
    bottom5 <- song_medians %>%
      filter(n >= 30) %>% arrange(desc(median_days)) %>% head(5) %>% pull(song_id)
    updateSelectizeInput(session, "searchme", selected = bottom5)
  })

  observeEvent(input$pick_clear, {
    updateSelectizeInput(session, "searchme", selected = character(0))
  })

  filtered_data <- reactive({
    req(input$searchme)
    filtered_by_year_df() %>%
      filter(song_id %in% input$searchme) %>%
      mutate(
        song = paste0(PageNum, " \u2013 ", Title),
        song = if (input$highlight_removed)
          paste0(song, ifelse(removed_2025, " \u2020", ""))
        else song,
        song = factor(song)
      )
  })

  # --- Survival plot ---
  output$survival_plot <- renderPlot({
    df <- filtered_data()
    fit <- survfit(Surv(time_to_event_days, event_status) ~ song, data = df)

    surv_tidy <- broom::tidy(fit) %>%
      mutate(strata = gsub("^song=", "", strata))

    ggplot(surv_tidy, aes(x = time, y = estimate, color = strata)) +
      geom_step(linewidth = 1.2) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_color_brewer(palette = "Set2") +
      labs(
        y = "Proportion Who Haven\u2019t Led the Song Yet",
        x = "Days Since First Hearing the Song",
        subtitle = "Faster drop = more \u2018contagious\u2019",
        color = "Song"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.subtitle = element_text(color = "grey50", size = 13),
        panel.grid.minor = element_blank()
      )
  })

  # --- Leaderboard table ---
  output$leaderboard_table <- renderDT({
    req(input$searchme)
    df <- filtered_data()

    tbl <- compute_median_survival(df) %>%
      arrange(median_days) %>%
      mutate(
        Rank = row_number(),
        Song = if (input$highlight_removed)
          paste0(PageNum, " \u2013 ", Title,
                 ifelse(PageNum %in% removed_pages, " \u2020", ""))
        else paste0(PageNum, " \u2013 ", Title),
        `Median Days to Lead` = round(median_days),
        `Exposures (n)` = n
      ) %>%
      select(Rank, Song, `Median Days to Lead`, `Exposures (n)`)

    datatable(
      tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = nrow(tbl)
      ),
      class = "compact stripe"
    )
  })

  # --- Full unfiltered table (Tab 2) ---
  output$full_table <- renderDT({
    survival_df %>%
      group_by(Page = PageNum, Song = Title) %>%
      summarise(
        `Median Days to Lead` = round(median(time_to_event_days, na.rm = TRUE)),
        `Exposures (n)`       = n(),
        `% Who Led`           = paste0(round(mean(event_status) * 100), "%"),
        .groups = "drop"
      ) %>%
      arrange(`Median Days to Lead`) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "ftipr"
        ),
        class = "compact stripe"
      )
  })
}

shinyApp(ui = ui, server = server)
