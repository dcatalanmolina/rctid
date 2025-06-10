one_match_df <- 
  function(
    all_matches, # df
    single_match # number
    ) {
  
  one_matchup <- 
    all_matches %>% 
    filter(matchid == all_match_id[single_match])
  
  preds_summary <- 
    one_matchup %>% 
    group_by(e_result) %>% 
    summarise(
      n = n(),
      pr = 100*(n/length(one_matchup$e_result))
    ) %>% 
    modify_if(is.numeric, ~round(., 2)) %>% 
    modify_at(
      .at = "e_result",
      ~factor(
        .x, labels = c("Draw", "Home Win", "Upset")
      )
    ) %>% 
    mutate(
      pr_txt = 
        paste(
          pr, "% ", e_result, sep = ""
        ),
      .epred = c(0, 1, -1),
      y = c(100, 200, 200)
    )
  
  one_output <- 
    list(
      one_matchup = one_matchup,
      preds_summary = preds_summary
    )
  
  return(one_output)
  
}