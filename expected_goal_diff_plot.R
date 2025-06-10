expected_goal_diff_plot <- 
  function(df, df_labs) {
  
    df %>% 
      ggplot(
        aes(x = .epred, fill = e_result)
      ) +
      geom_histogram(bins = 100) +
      geom_text(
        data = df_labs,
        aes(
          x = .epred, y = y, label = pr_txt
        )
      ) +
      labs(
        x = "Expected Goal Difference",
        y = ""
      ) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
}