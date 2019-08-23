# NHL PLAYERS SEASON STATS SCRAPE ----

# Libraries ----
library(tidyverse)
library(glue)
library(rvest)
library(fs)

# Source Functions ----
source("player_scrape.R")

# Source Settings ----
source("settings.R")

# Generate Stats Data ----
players_raw_tbl <- create_player_stats_tbl(season_start, season_end)

# Process Player Stats ----
players_tbl <- process_players_stats(players_raw_tbl)

# Export Data ----

# if the data directory doesn't exist, create it
if (!dir_exists(data_dir)) {
  
  dir_create(data_dir)
}

# save file
write_csv(players_tbl, fs::path(data_dir, stats_file_name))



