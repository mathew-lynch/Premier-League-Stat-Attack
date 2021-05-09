# Calculate win/draw/loss totals and percentages for home and away games for EVERY team - this is required
# when calculating team rankings

summary_all <- function(full_results, team_names) {
  processed_results <- data.frame()
  
  for (i in 1:length(team_names)) {
    filtered <- full_results[full_results$TEAM_NAME == team_names[i],]
    processed <- process_data(filtered, team_names[i])
    processed$TEAM_NAME <- rep(team_names[i], nrow(processed))
    processed_results <- rbind(processed_results, processed)
  }
  
  return(processed_results)
}
