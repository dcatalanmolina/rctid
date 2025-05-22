######################################
# Getting 2025 Football Data
#####################################

library(devtools)
# get worldfootballR
devtools::install_github("JaseZiv/worldfootballR")

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

saveRDS(mls_25_defense, "data/mls_25_defense.rds")