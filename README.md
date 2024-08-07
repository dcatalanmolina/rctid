# rctid
Code to get and analyze Portland Timbers' and Thorns' stats.

## Get data
- From {worldfootballR}: https://usfootballr.sportsdataverse.org/index.html](https://github.com/JaseZiv/worldfootballR
- From espn: (MLS) http://site.api.espn.com/apis/site/v2/sports/soccer/usa.1

### Example 
```r
library(httr2)
library(worldfootballR)
library(tidyverse)

# Fbref data
mls_matches <-
  fb_match_results(country = "USA", gender = "M", season_end_year = 2024, tier = "1st")

# ESPN data
espn_mls_scoreboard <- 
  "http://site.api.espn.com/apis/site/v2/sports/soccer/usa.1"

req_espn <- 
  request(espn_mls_scoreboard)
  
mls <- 
  req_espn %>% 
  req_perform()

mls_json <- 
  mls %>% 
  resp_body_json()

```
