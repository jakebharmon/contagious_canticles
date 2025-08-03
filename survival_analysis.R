# # Load packages
library(DBI)
library(RSQLite)
library(tidyverse)

# Connect to SQl Database
con <- dbConnect(SQLite(), "minutes.db")
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
  mutate(date = mdy(Date),
         song = paste0(PageNum, " - ", Title)) %>% 
  select(-c(Date,PageNum,Title)) %>% 
  drop_na()

# This is the date the person first led a specific song
first_sung_df = minutes %>%
  group_by(leader_id, song_id, song) %>%
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
final_df = full_join(exposure_df, first_sung_df, by = c("leader_id", "song_id")) %>%
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
  # Clean up the column names to match the requested format
#  select(
#    `Person ID` = leader_id,
#    `Song ID` = song_id,
#    `Date of First Exposure` = date_first_exposure,
#    `Date of First Time Sung` = date_first_sung,
#    `Time to Event (Days)` = time_to_event_days,
#    `Event Status (0/1)` = event_status
#  )

### Survival analysis

library(survival)
library(survminer)
library(plotly)

survival_df = final_df %>% 
  select(song_id, event_status, time_to_event_days, song)

sfit = survfit(Surv(time_to_event_days, event_status) ~ song, data = survival_df)
p = ggsurvplot(sfit, legend = "none")
ggplotly(p$plot)
