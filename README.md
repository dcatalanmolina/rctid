# rctid
Code to get and analyze Portland Timbers' and Thorns' stats.

## Get data
- From {usfootballR}: https://usfootballr.sportsdataverse.org/index.html
- From espn: (MLS) http://site.api.espn.com/apis/site/v2/sports/soccer/usa.1

### Example 
```r
library(httr2)
library(tidyverse)

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
