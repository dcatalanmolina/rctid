expected_standings <- 
  function(
    df_mls, #mls_wide
    df_future, #future_matches
    m #match preds
    ) {
    
    # west string
    west_conf <- 
      c(
        "LA Galaxy", "LAFC", "RSL", "Rapids",
        "Vancouver W'caps", "Austin", "Dynamo FC",
        "FC Dallas", "Minnesota Utd", "Portland Timbers",
        "SJ Earthquakes", "Seattle", "Sporting KC","St. Louis"
      )
    
    # expected team points for the rest of the season
    future_matches_preds <- 
      left_join(
        future_matches,
        m,
        by = c("Wk", "matchup")
      )
    
    future_home_points <- 
      future_matches_preds %>% 
      rename(team = Home) %>% 
      group_by(team) %>% 
      summarise(
        e_points = sum(e_home_points)
      )
    
    future_away_points <- 
      future_matches_preds %>% 
      rename(team = Away) %>% 
      group_by(team) %>% 
      summarise(
        e_points = sum(e_away_points)
      )
    
    future_points <- 
      bind_rows(
        future_home_points,
        future_away_points
      ) %>% 
      group_by(team) %>% 
      summarise(
        e_future_points = sum(e_points)
      )
    
    # actual points thus far
    actual_home_points <- 
      df_mls %>% 
      rename(team = Home) %>% 
      group_by(team) %>% 
      summarise(
        a_points = sum(a_home_points, na.rm = T)
      )
    
    actual_away_points <- 
      df_mls %>% 
      rename(team = Away) %>% 
      group_by(team) %>% 
      summarise(
        a_points = sum(a_away_points, na.rm = T)
      )
    
    total_points <- 
      bind_rows(
        actual_home_points,
        actual_away_points
      ) %>% 
      group_by(team) %>% 
      summarise(
        a_total_points = sum(a_points)
      )
    
    predicted_standings <- 
      left_join(
        total_points,
        future_points,
        by = "team"
      ) %>% 
      rowwise() %>% 
      mutate(
        e_total = sum(c(a_total_points, e_future_points), na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        conference = 
          if_else(team %in% west_conf, "West", "East")
      )
    
  
}