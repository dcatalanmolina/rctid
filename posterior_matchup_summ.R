posterior_matchup_summ <- 
  function(df) {
    
    preds_df <- 
      df %>% 
      group_by(Wk, matchup) %>% 
      summarise(
        e_goal_diff = quantile(.epred, probs = .50),
        e_goal_ll = quantile(.epred, probs = .055),
        e_goal_ul = quantile(.epred, probs = .945),
        pr_home_win = mean(if_else(.epred > 0.5, 1, 0)),
        pr_upset = mean(if_else(.epred < -0.5, 1, 0)),
        e_home_points = 
          case_when(
            pr_upset > .60 ~ 0,
            pr_upset < .40 & pr_home_win > .50 ~ 3,
            TRUE ~ 1
          ),
        e_away_points = 
          case_when(
            e_home_points == 0 ~ 3,
            e_home_points == 3 ~ 0,
            TRUE ~ 1
          )
      )
    
    return(preds_df)
    
  }
