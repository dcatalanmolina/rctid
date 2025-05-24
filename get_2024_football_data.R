######################################
# Getting 2024 Football Data
#####################################

library(httr2)
library(tidyverse)
library(worldfootballR)

# Match Results from FBref ------------------------

## MLS -----
mls_24 <- 
  fb_match_results(
    country = "USA", gender = "M", season_end_year = 2024, tier = "1st"
  )

saveRDS(mls_24, "data/mls_24.rds")

# Season-level stats from FBref -------------------

## MLS
mls_24_defense <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2024, tier = "1st",
    stat_type = "defense"
  )

mls_24_shooting <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2024, tier = "1st",
    stat_type = "shooting"
  )

mls_24_standard <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2024, tier = "1st",
    stat_type = "standard"
  )

saveRDS(mls_24_defense, "data/mls_24_defense.rds")
saveRDS(mls_24_shooting, "data/mls_24_shooting.rds")
saveRDS(mls_24_standard, "data/mls_24_standard.rds")
