#############################
## Modeling MLS Matches ######
############################

# 1. Libraries and data ---------
library(tidyverse)
library(brms)

mls_wide <- readRDS("data/mls_wide.rds")


# 2.  WYM Model (win your matchup, interaction)  ---------
m2.4 <-
  brm(
    goal_diff ~ 1 + home_edge*away_edge + (1|Home) + (1|Away),
    data = mls_wide,
    warmup = 1500,
    iter = 8000,
    seed = 3
  )

saveRDS(
  m2.4, 
  paste("models/m2_4.rds", sep = "")
)
