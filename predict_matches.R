#############################
## Predict MLS Matches ######
############################

# 1. Libraries, data, and model outputs ---------
library(tidyverse)
library(brms)
library(tidybayes)

m2 <- readRDS("models/m2.rds")
m2.1 <- readRDS("models/m2_1.rds")

# funs
source("posterior_matchup_summ.R")
source("expected_standings.R")

# get match df
mls_wide <- 
  readRDS("data/mls_wide.rds") %>% 
  filter(Season_End_Year == "2025") %>% 
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

# two sets of expected values, based on different models
future_preds_m2 <- 
  add_epred_draws(
    future_matches,
    m2
  )

future_preds_m2.1 <- 
  add_epred_draws(
    future_matches,
    m2.1
  )

saveRDS(future_preds_m2, paste("models/m2_future_preds_", today(), ".rds", sep = ""))
saveRDS(future_preds_m2.1, paste("models/m21_future_preds_", today(), ".rds", sep = ""))

# posterior summaries
future_preds_m2_summary <- 
  posterior_matchup_summ(future_preds_m2)
 
future_preds_m21_summary <- 
  posterior_matchup_summ(future_preds_m2.1)

saveRDS(future_preds_m2_summary, paste("models/m2_future_preds_summary", today(), ".rds", sep = ""))
saveRDS(future_preds_m21_summary, paste("models/m21_future_preds_summary", today(), ".rds", sep = ""))

standings_m2 <- 
  expected_standings(
    mls_wide, future_matches, 
    future_preds_m2_summary
  )

standings_m21 <- 
  expected_standings(
    mls_wide, future_matches, 
    future_preds_m21_summary
  )

saveRDS(standings_m2, "models/standings_m2.rds")
saveRDS(standings_m21, "models/standings_m21.rds")
