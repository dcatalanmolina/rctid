#############################
## Cleaning Data ######
############################

# 1. Libraries and data ---------
library(tidyverse)
library(brms)

# home mismatch
home_mismatch <- function(home, away, year) {
  
  home_stats <- 
    squad_q %>% 
    filter(Squad == home & Season_End_Year == year)
  
  away_stats <- 
    squad_q %>% 
    filter(Squad == away & Season_End_Year == year)
  
  mismatch <- home_stats$off - away_stats$def
  # positive number is home offensive edge over away defense 
  return(mismatch)
  
}

# away mismatch
away_mismatch <- function(home, away, year) {
  
  home_stats <- 
    squad_q %>% 
    filter(Squad == home & Season_End_Year == year)
  
  away_stats <- 
    squad_q %>% 
    filter(Squad == away & Season_End_Year == year)
  
  mismatch <- away_stats$off - home_stats$def
  # positive number is away offensive edge over home defense 
  return(mismatch)
  
}


mls_24 <- readRDS("data/mls_24.rds")
mls_24_defense <- readRDS("data/mls_24_defense.rds")
mls_24_shooting <- readRDS("data/mls_24_shooting.rds")
mls_24_standard <- readRDS("data/mls_24_standard.rds")

mls_25 <- readRDS("data/mls_25.rds")
mls_25_defense <- readRDS("data/mls_25_defense.rds")
mls_25_shooting <- readRDS("data/mls_25_shooting.rds")
mls_25_standard <- readRDS("data/mls_25_standard.rds")

# 2. Clean -----------------------
## squad stats represent offense for each team, whereas opponent stats
## represent offense allowed by each team (defense). 
## this is different from home and away teams. 
## squad stats AND opponent stats should feed into off_q and def_q


## possession time, goals, and assists
mls_standard_summary <- 
  bind_rows(
    mls_24_standard, mls_25_standard
  ) %>% 
  select(
    Season_End_Year, Squad, Poss, Gls_Per_Minutes, Ast_Per_Minutes
  ) %>% 
  modify_at(
    .at = c("Gls_Per_Minutes", "Ast_Per_Minutes"),
    ~(.x - mean(.x, na.rm = T))/sd(.x, na.rm = T) # allows good teams to be higher away and at home than bad teams
  )

squad_offense <- 
  mls_standard_summary %>% 
  filter(!str_detect(Squad, "vs ")) %>% 
  rowwise() %>% 
  mutate(
    off = mean(c(Gls_Per_Minutes, Ast_Per_Minutes), na.rm = T)
  ) %>% 
  select(Season_End_Year, Squad, Poss, off)

squad_offense_against <- 
  mls_standard_summary %>% 
  filter(str_detect(Squad, "vs ")) %>% 
  mutate(Squad = str_remove(Squad, "vs ")) %>% 
  rowwise() %>% 
  mutate(
    off_against = mean(c(Gls_Per_Minutes, Ast_Per_Minutes), na.rm = T)*-1
    # lower number is worse defense
  ) %>% 
  select(
    Season_End_Year, Squad, off_against
  )


## tackles, blocks, and errors
key_def_stats <- c("TklW_Tackles", "Blocks_Blocks", "Err")

mls_defense_summary <- 
  bind_rows(
    mls_24_defense, mls_25_defense
  )  %>% 
  select(
    Season_End_Year, Squad, Mins_Per_90, all_of(key_def_stats)
  ) %>% 
  mutate(
    tklw_game = TklW_Tackles/Mins_Per_90, # these need to be weighted by inv of possession time
    blocks_game = Blocks_Blocks/Mins_Per_90,
    err_game = (Err/Mins_Per_90)*-1 # fewer errors, better defense
  ) %>% 
  modify_at(
    .at = c("tklw_game", "blocks_game", "err_game"),
    ~(.x - mean(.x, na.rm = T))/sd(.x, na.rm = T) # allows good teams to be higher away and at home than bad teams
  ) 

squad_defense <- 
  mls_defense_summary %>% 
  filter(!str_detect(Squad, "vs ")) %>% 
  rowwise() %>% 
  mutate(
    def = mean(c(tklw_game, blocks_game, err_game), na.rm = T)
  ) %>% 
  select(
    Season_End_Year, Squad, def
  )

squad_q <- 
  left_join(
    squad_offense,
    squad_defense,
    by = c("Season_End_Year", "Squad")
  )

## merge stats
mls_wide <- 
  mls_24 %>% 
  select(
    Season_End_Year, Wk, Date, Home, HomeGoals, Away, AwayGoals
  ) %>% 
  mutate(
    goal_diff = HomeGoals - AwayGoals,
    home_edge = 
      pmap_dbl(
        list(
          a = Home, b = Away, c = Season_End_Year
        ),
        ~home_mismatch(home = a, away = b, year = c)
      ),
    away_edge = 
      pmap_dbl(
        list(
          a = Home, b = Away, c = Season_End_Year
        ),
        ~away_mismatch(home = a, away = b, year = c)
      )
  ) 

saveRDS(mls_wide, "data/mls_wide.rds")
