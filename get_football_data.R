######################################
# Getting Football Data
#####################################

library(httr2)
library(tidyverse)
library(worldfootballR)

# Team details ------------------
## MLS -------------------
espn_mls_team_details <- 
  "http://site.api.espn.com/apis/site/v2/sports/soccer/usa.1/teams"

### espn, timbers id = 9723
### mls, opta_id = 1581

req_mls <- 
  request(
    espn_mls_team_details
  )

mls <- 
  req_mls %>% 
  req_perform()

mls_json <- 
  mls %>% 
  resp_body_json()

## NWSL -------------------
espn_nwsl_team_details <- 
  "http://site.api.espn.com/apis/site/v2/sports/soccer/usa.nwsl/teams"

req_nwsl <- 
  request(espn_nwsl_team_details)

nwsl <- 
  req_nwsl %>% 
  req_perform()

nwsl_json <- 
  nwsl %>% 
  resp_body_json()

## TIMBERS -----------------
timbers_stats <- 
  "http://site.api.espn.com/apis/site/v2/sports/soccer/usa.1/teams/9723"

req_timbers <- 
  request(timbers_stats)

timbers <- 
  req_timbers %>% 
  req_perform()

timbers_json <- 
  timbers %>% 
  resp_body_json()


## THORNS ------------------
thorns_stats <- 
  "http://site.api.espn.com/apis/site/v2/sports/soccer/usa.nwsl/teams/15362"

### thorns id = 15362
req_thorns <- 
  request(thorns_stats)

thorns <- 
  req_thorns %>% 
  req_perform()

thorns_json <- 
  thorns %>% 
  resp_body_json()

### inside`team`, `record$items[[1]]` contains a `stats` object with a 
### few stats 

# Match Results from FBref ------------------------

## MLS -----
mls_24 <- 
  fb_match_results(
    country = "USA", gender = "M", season_end_year = 2024, tier = "1st"
  )

saveRDS(mls_24, "data/mls_24.rds")
