#############################
## Explore Predictions ######
############################

# 1. Libraries, data, and model outputs ---------
library(tidyverse)
library(brms)
library(tidybayes)

m2 <- readRDS("models/m2.rds")
m2.1 <- readRDS("models/m2_1.rds")

# 2. Add draws to past data ---------------------
past_matches_m2 <- 
  m2[["data"]] %>% 
  add_epred_draws(m2)

past_matches_m21 <- 
  m2.1[["data"]] %>% 
  add_epred_draws(m2.1)

# 3. Summary ------------------------------------
m2_accuracy <- 
  past_matches_m2 %>% 
  mutate(
    matchup = paste(Home, " vs ", Away, ": ", goal_diff, sep = ""),
    accuracy_50 = 
      if_else(
        goal_diff - .epred < abs(.50),
        1,
        0
      )
  ) %>% 
  group_by(matchup) %>% 
  summarise(
    m_accuracy = mean(accuracy_50),
    goal_diff = median(goal_diff),
    m_egoal_diff = mean(.epred),
    ci_egoal_diff_lo = quantile(.epred, probs = .055),
    ci_egoal_diff_hi = quantile(.epred, probs = .945)
  )

m2_accuracy %>% 
  ggplot(
    aes(
      x = matchup, y = goal_diff, 
      ymin = ci_egoal_diff_lo,
      ymax = ci_egoal_diff_hi,
      color = if_else(goal_diff > 2, "b", "t")
      )
  ) +
  geom_point() +
  geom_errorbar() +
  #coord_flip() +
  theme(
    axis.text.x = element_blank()
  )

m21_accuracy <- 
  past_matches_m21 %>% 
  mutate(
    matchup = paste(Home, " vs ", Away, ": ", goal_diff, sep = ""),
    accuracy_50 = 
      if_else(
        goal_diff - .epred < abs(.50),
        1,
        0
      )
  ) %>% 
  group_by(matchup) %>% 
  summarise(
    m_accuracy = mean(accuracy_50),
    tight_game = 
      if_else(
        median(goal_diff) < abs(2), 
        "tight", "blowout"
        )
  )

m21_accuracy %>% 
  ggplot(
    aes(
      x = m_accuracy, 
      fill = tight_game, 
      group = tight_game 
      )
    ) +
  geom_histogram()

## notes: the baseline model is not accurately predicting 
### goal differences over 2. Yet the model is very confident
### predicting draws and tight matches (1 goal diff).

### neither model is good at predicting home team dominated games
### (goal_diff > 2).

# how much more accurate is m2.1?
left_join(
  m2_accuracy,
  m21_accuracy,
  by = "matchup"
) %>% 
  mutate(
    m21_edge = m_accuracy.y - m_accuracy.x
  ) %>% 
  group_by(tight_game) %>% 
  summarise(
    m = mean(m21_edge),
    sd = sd(m21_edge),
    ci_25 = quantile(m21_edge, probs = .25),
    ci_75 = quantile(m21_edge, probs = .75)
  )

## notes: m2.1 is marginally better at predicting
### blowout games (max 1%). In practice, these models
### are making the same predictions.


