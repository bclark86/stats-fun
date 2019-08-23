# NHL PLAYER SCRAPE UTILITIES ----

# Libraries ----
library(rvest)
library(tidyverse)

# Player Table Columns ----
player_table_cols <- c(
  "player",
  "age",
  "team",
  "position",
  "gp",
  "goals",
  "assists",
  "points",
  "plus_minus",
  "pims",
  "point_shares",
  "ev_goals",
  "pp_goals",
  "sh_goals",
  "gw_goals",
  "ev_assists",
  "pp_assists",
  "sh_assists",
  "shots",
  "shot_pct",
  "toi",
  "atoi",
  "blocks",
  "hits",
  "fow",
  "fol",
  "fo_pct"
)


# Scrape Stats Functions ----

# function to scrape player stats table for single season
get_player_stats <- function(season) {
  
  url <- sprintf(
    "https://www.hockey-reference.com/leagues/NHL_%d_skaters.html",
    season
  )
  
  season_page <- read_html(url)
  
  stats_table <- season_page %>%
    html_nodes("#stats") %>%
    html_table()
  
  stats_table <- stats_table[[1]]
  
  if (season < 2008){
    
    player_tbl <- stats_table %>% 
      rownames_to_column() %>%
      select(3:ncol(.)) %>%
      `colnames<-`(player_table_cols[1:22]) %>%
      as.tibble() %>%
      filter(player != "Player",
             team != "TOT") %>%
      mutate(season = season) %>%
      select(season, everything(), -atoi) %>%
      mutate_at(
        c(3, 6:11, 13:20, 21), as.integer
      ) %>%
      mutate_at(
        c(12, 21), as.numeric
      )
    
  } else {
    
    player_tbl <- stats_table %>% 
      rownames_to_column() %>%
      select(3:ncol(.)) %>%
      `colnames<-`(player_table_cols) %>%
      as.tibble() %>%
      filter(player != "Player",
             team != "TOT") %>%
      mutate(season = season) %>%
      select(season, everything(), -atoi, -fo_pct) %>%
      mutate_at(
        c(3, 6:11, 13:20, 21, 23:26), as.integer
      ) %>%
      mutate_at(
        c(12, 21), as.numeric
      )
    
  }
  
  return(player_tbl)
  
}

# wrap function in "possibly" to continue if error
possible_player_stats <- possibly(get_player_stats, otherwise = NA_real_)

# # function to scrape player stats table for single season
create_player_stats_tbl <- function(season_start, season_end, sleep = 5) {
  
  seasons_tbl <- tibble(
    season = season_start:season_end
  ) %>% 
    mutate(
      stats = map(season, ~ {
        Sys.sleep(sleep)
        message(glue("Scraping the {.x} season"))
        possible_player_stats(.x)
      })
    )
  
  seasons_tbl <- seasons_tbl %>%
    filter(!is.na(stats)) %>%
    select(stats) %>%
    map_df(bind_rows)
  
  return(seasons_tbl)
}


# Process Player Stats ----

process_players_stats <- function(players_raw_tbl) {
  
  players_tbl <- players_raw_tbl %>%
    group_by(season, player, age, position) %>%
    summarize(
      teams        = paste(team, collapse = "/"),
      gp           = sum(gp),
      goals        = sum(goals),
      assists      = sum(assists),
      points       = sum(points),
      plus_minus   = sum(plus_minus),
      pims         = sum(pims),
      point_shares = sum(point_shares),
      ev_goals     = sum(ev_goals),
      pp_goals     = sum(pp_goals),
      sh_goals     = sum(sh_goals),
      gw_goals     = sum(gw_goals),
      ev_assists   = sum(ev_assists),
      pp_assists   = sum(pp_assists),
      sh_assists   = sum(sh_assists),
      shots        = sum(shots),
      shot_pct     = goals / shots,
      toi          = sum(as.integer(toi)),
      blocks       = sum(as.integer(blocks)),
      hits         = sum(as.integer(hits)),
      fow          = sum(as.integer(fow)),
      fol          = sum(as.integer(fol))
    ) %>%
    ungroup() %>%
    mutate(
      faceoffs = fow + fol,
      fo_pct   = fow / faceoffs,
      esp_60   = (ev_goals + ev_assists) / toi * 60,
      esg_60   = ev_goals / toi * 60,
      esa_60   = ev_assists / toi * 60,
    )
  
  return(players_tbl)
  
} 
