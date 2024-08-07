#############################
## Modeling MLS Matches ######
############################

# 1. Libraries and data ---------
library(tidyverse)
library(brms)

mls_24 <- readRDS("data/mls_24.rds")

# 2. Clean and explore data -----------

### wide df, each row is a match
mls_wide <- 
  mls_24 %>% 
  select(
    Wk, Home, HomeGoals, Away, AwayGoals
  ) %>% 
  mutate(
    goal_diff = HomeGoals - AwayGoals
  )

# how many goals do each home allows?

## long df, each row is a team within a match
home_goals <- 
  mls_24 %>% 
  select(
    Wk, Home, HomeGoals, 
  ) %>% 
  rename(
    team = Home, 
    goals = HomeGoals
  ) %>% 
  mutate(
    where = "home",
    game_id = 1:length(.$team)
  )

away_goals <- 
  mls_24 %>% 
  select(
    Wk, Away, AwayGoals
  ) %>% 
  rename(
    team = Away, 
    goals = AwayGoals
  ) %>% 
  mutate(
    where = "away",
    game_id = 1:length(.$team)
  )

mls_long <- 
  bind_rows(
    home_goals,
    away_goals
  )

## explore data ----------------------------
mls_wide %>% 
  mutate(
    home_win = if_else(goal_diff > 0, 1, 0),
    upset = if_else(goal_diff < 0, 1, 0),
    tie = if_else(goal_diff == 0, 1, 0)
  ) %>% 
  summarise_at(
    .vars = c("home_win", "upset", "tie"),
    ~mean(., na.rm = T)
  )

# 3. Empty model, predicting goals ---------
m0 <- 
  brm(
    goals ~ 1 + (1|team),
    data = mls_long,
    warmup = 100,
    iter = 1000,
    seed = 3
  )

# 4. Home field advantage ------
m1 <- 
  brm(
    goals ~ 1 + where + (1|team),
    data = mls_long,
    warmup = 100,
    iter = 1000,
    seed = 3
  )

# predictions
next_match <- 
  tibble(
    team = c("Portland Timbers", "St. Louis"),
    where = c("away", "home")
  )

predict(m1, newdata = next_match)

## notes: this is cool, it helps to predict goals, but it doesn't take
## into account the matchup. it may be better to keep the wide df and 
## predict goal diff and use home/away as crossed clusters.

# 4. Matchup model ---------
m2 <- 
  brm(
    goal_diff ~ 1 + (1|Home) + (1|Away),
    data = mls_wide,
    warmup = 100,
    iter = 1000,
    seed = 3
  )

saveRDS(m2, "models/m2.rds")
