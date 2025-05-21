#############################
## Cleaning Data ######
############################

# 1. Libraries and data ---------
library(tidyverse)
library(brms)

mls_24 <- readRDS("data/mls_24.rds")
mls_24_defense <- readRDS("data/mls_24_defense.rds")

mls_25 <- readRDS("data/mls_25.rds")
mls_25_defense <- readRDS("data/mls_25_defense.rds")

# 2. Clean -----------------------
mls_defense_summary <- 
  bind_rows(
    mls_24_defense, mls_25_defense
  ) %>% 
  select(
    Season_End_Year, Squad, Mins_Per_90, TklW_Tackles, Tkl_percent_Challenges, Err
  )

away_defense_summary <- 
  mls_defense_summary %>% 
  filter(str_detect(Squad, "vs ")) %>% 
  rename(Away = Squad) %>%
  mutate(Away = str_remove(Away, "vs ")) %>% 
  modify_at(
    .at = c("TklW_Tackles", "Tkl_percent_Challenges", "Err"),
    ~(.x - mean(.x))/sd(.x)
  )

mls_wide <- 
  bind_rows(
    mls_24, mls_25
  ) %>% 
  select(
    Season_End_Year, Wk, Date, Home, HomeGoals, Away, AwayGoals
  ) %>% 
  mutate(
    goal_diff = HomeGoals - AwayGoals
  ) %>% 
  left_join(
    ., away_defense_summary,
    by = c("Season_End_Year", "Away")
  )

saveRDS(mls_wide, "data/mls_wide.rds")
