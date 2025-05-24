#############################
## Predict MLS Matches ######
############################

# 1. Libraries, data, and model outputs ---------
library(tidyverse)
library(brms)
library(tidybayes)

m_today <- readRDS("models/m2_4.rds")

# funs
source("posterior_matchup_summ.R")
source("expected_standings.R")

# get match df
mls_wide <- 
  readRDS("data/mls_wide.rds") %>% 
  mutate(
    a_home_points = #actual
      case_when(
        goal_diff == 0 ~ 1, #draw
        goal_diff > 0 ~ 3, #win
        goal_diff < 0 ~ 0, #lose
        T ~ NA_integer_ #future match
      ),
    a_away_points = 
      case_when(
        goal_diff == 0 ~ 1, #draw
        goal_diff < 0 ~ 3, #win
        goal_diff > 0 ~ 0, #lose
        T ~ NA_integer_ #future match
      )
  )


# get future matches
future_matches <- 
  mls_wide %>% 
  filter(Date > today()) %>% 
  mutate(matchup = paste(Home, "vs", Away, sep = " "))

# 2. Predict all remaining matches --------------

# expected values
future_preds <- 
  add_epred_draws(
    future_matches,
    m_today
  )

saveRDS(
  future_preds, 
  paste("models/m2.4_future_preds.rds", sep = ""))


# posterior summaries
future_preds_summary <- 
  posterior_matchup_summ(future_preds)
 
standings <- 
  expected_standings(
    mls_wide, future_matches, 
    future_preds_summary
  )

saveRDS(standings, "models/m2.4_standings.rds")
