#############################
## Predict MLS Matches ######
############################

# 1. Libraries and model outputs ---------
library(tidyverse)
library(brms)
library(tidybayes)



m2 <- readRDS("models/m2.rds")

# get future matches
future_matches <- 
  readRDS("data/mls_24.rds") %>% 
  filter(Date > today())

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

# 3. Predict next week --------------
week_28 <- 
  future_matches %>% 
  filter(Wk == "28") %>% 
  mutate(matchup = paste(Home, "vs", Away, sep = " "))

week_28_preds <- 
  add_predicted_draws(
    week_28,
    m2
  )

posterior_matchup_preds <- 
  function(df) {
    
    pred_distribution <- 
      quantile(df$.prediction, probs = c(.10, .25, .50, .75, .90))
    
    matchup_probs <- 
      df %>% 
      summarise(
        pr_home_win = mean(if_else(.prediction > 0, 1, 0)),
        pr_upset = mean(if_else(.prediction < 0, 1, 0))
      )
    
    pred_summary <- 
      list(
        pred_distribution = pred_distribution,
        matchup_probs = matchup_probs
      )
    
    return(pred_summary)
  
}

week_28_preds_summary <- 
  map(
    .x = unique(week_28$matchup),
    ~week_28_preds %>% 
      filter(matchup == .x) %>% 
      posterior_matchup_preds(.)
  )

names(week_28_preds_summary) <- unique(week_28$matchup)

saveRDS(week_28_preds_summary, "models/m2_wk28_preds.rds")

