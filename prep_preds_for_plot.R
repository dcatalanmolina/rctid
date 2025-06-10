prep_preds_for_plot <- 
  function(preds_list) {
  
    preds_for_plot <- 
      preds_list %>% 
      mutate(
        matchid = paste(matchup, Date, sep = " - "),
        e_result = 
          case_when(
            .epred > 0.5 ~ "home_win",
            .epred < -0.5 ~ "upset",
            TRUE ~ "draw"
          )
      )
    
    all_match_id <- unique(preds_for_plot$matchid)  
    
    preds_n_ids <- 
      list(
        clean_preds = preds_for_plot,
        id = all_match_id
      )
    
    return(preds_n_ids)
    
}