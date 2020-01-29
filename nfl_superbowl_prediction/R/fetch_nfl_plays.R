library(nflscrapR)
library(readr)
library(glue)
library(fs)
library(data.table)


fetch_nfl_plays <- function(season = 2019, 
                            type = c("reg", "post"),
                            data_dir = "data") {
  
  file_path = fs::path(data_dir, 
                       glue::glue("nfl_season_pbp_{season}"),
                       ext = "csv")
  
  if (!fs::file_exists(file_path)) {
    
    all_games = tibble()
    
    for (game_type in type) {
      game_plays <- scrape_season_play_by_play(season, type = game_type)
      all_games  <- data.table::rbindlist(list(all_games, game_plays)) %>% as_tibble()
    }
    
    readr::write_csv(all_games, file_path)
  }
  
  nfl_data <- readr::read_csv(file_path)
  
  return(nfl_data)
}
