#############################
## Predict MLS Matches ######
############################

# 1. Libraries and model outputs ---------
library(tidyverse)
library(brms)
library(tidybayes)

m2 <- readRDS("models/m2.rds")

# 2. Predict single match ---------------
next_match <- 
  tibble(
    Home = "St. Louis",
    Away = "Portland Timbers"
  )

pred_match <- 
  add_predicted_draws(
    next_match,
    m2
  )

## Explore predictions
summary(pred_match$.prediction)
quantile(pred_match$.prediction, probs = c(.25, .75))

pred_match %>% 
  summarise(
    pr_home_win = mean(if_else(.prediction > 0, 1, 0)),
    pr_upset = mean(if_else(.prediction < 0, 1, 0))
  )
