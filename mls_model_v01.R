#############################
## Modeling MLS Matches ######
############################

# 1. Libraries and data ---------
library(tidyverse)
library(brms)

mls_wide <- readRDS("data/mls_wide.rds")


# 2. Empty Matchup Model ---------
m2 <- 
  brm(
    goal_diff ~ 1 + (1|Home) + (1|Away),
    data = mls_wide,
    warmup = 2500,
    iter = 5000,
    seed = 3
  )

saveRDS(m2, "models/m2.rds")

# 3. Matchup Model ---------

m2.1 <- 
  brm(
    goal_diff ~ 1 + Tkl_percent_Challenges*Err + (1|Home) + (1|Away),
    data = mls_wide,
    warmup = 2500,
    iter = 5000,
    seed = 3
  )

saveRDS(m2.1, "models/m2_1.rds")
