library(tidyverse)
library(survival)
library(RSQLite)

base_data <- readRDS("~/Documents/sacred_harp_km/survival_data.Rds")

survival_data <- survfit(Surv(time_to_event_days, event_status) ~ song_id, data = survival_data)

# Access the summary table of the survfit object
summary_table <- summary(survival_data)$table %>% 
  as_tibble() %>% 
  mutate(row_id = row_number()) %>% 
  full_join(base_data, by = join_by("row_id" == "song_id")) %>% 
  select(Title, records, median) %>% 
  distinct()

write_csv(summary_table, "cc_median_survival_curve_summary.csv")


###
# Connect to SQl Database
con = dbConnect(SQLite(), "minutes.db")

# Create table of all data tables
tables = as.data.frame(dbListTables(con))

# Query combing song data
sql_minutes = con %>% dbGetQuery(
  "SELECT *
  FROM leaders")
