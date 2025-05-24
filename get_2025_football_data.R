######################################
# Getting 2025 Football Data
#####################################

# get worldfootballR
pak::pkg_install("JaseZiv/worldfootballR")

library(worldfootballR)

# Match Results from FBref ------------------------

## MLS -----
mls_25 <- 
  fb_match_results(
    country = "USA", gender = "M", season_end_year = 2025, tier = "1st"
  )

saveRDS(mls_25, "data/mls_25.rds")

# Season-level stats from FBref -------------------

## MLS
mls_25_defense <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2025, tier = "1st",
    stat_type = "defense"
  )

mls_25_shooting <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2025, tier = "1st",
    stat_type = "shooting"
  )

mls_25_standard <- 
  fb_season_team_stats(
    country = "USA", gender = "M", 
    season_end_year = 2025, tier = "1st",
    stat_type = "standard"
  )

saveRDS(mls_25_defense, "data/mls_25_defense.rds")
saveRDS(mls_25_shooting, "data/mls_25_shooting.rds")
saveRDS(mls_25_standard, "data/mls_25_standard.rds")

