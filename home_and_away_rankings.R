# Calculate home rankings for wins, draws and losses
home_rankings <- function(processed_results, team_names) {
  home_rankings <- data.frame()
  
  for (j in 1:length(team_names)) {
    hw <- home_wins(processed_results[processed_results$TEAM_NAME == team_names[j],])
    hd <- home_draws(processed_results[processed_results$TEAM_NAME == team_names[j],])
    hl <- home_losses(processed_results[processed_results$TEAM_NAME == team_names[j],])
    h <- data.frame("TEAM" = rep(team_names[j], 3), "OUTCOME" = c("WIN", "DRAW", "LOSS"), "PERCENTAGE" = c(hw, hd, hl))
    home_rankings <- rbind(home_rankings, h)
  }
  
  # Add rank
  home_rankings <- home_rankings %>%
    group_by(OUTCOME) %>%
    arrange(OUTCOME, desc(PERCENTAGE)) %>%
    # Wins and draws are ranked best to worst, with rankings being reversed for losses (i.e. the team
    # with the fewest losses is ranked first, rather than last)
    mutate(RANK = case_when(OUTCOME %in% c("WIN", "DRAW") ~ rank(-PERCENTAGE, ties.method = "min"),
                            OUTCOME %in% c("LOSS") ~ rank(PERCENTAGE, ties.method = "min")))
  
  return(home_rankings)
}

# Calculate away rankings for wins, draws and losses
away_rankings <- function(processed_results, team_names) {
  away_rankings <- data.frame()
  
  for (j in 1:length(team_names)) {
    aw <- away_wins(processed_results[processed_results$TEAM_NAME == team_names[j],])
    ad <- away_draws(processed_results[processed_results$TEAM_NAME == team_names[j],])
    al <- away_losses(processed_results[processed_results$TEAM_NAME == team_names[j],])
    a <- data.frame("TEAM" = rep(team_names[j], 3), "OUTCOME" = c("WIN", "DRAW", "LOSS"), "PERCENTAGE" = c(aw, ad, al))
    away_rankings <- rbind(away_rankings, a)
  }
  
  # Add rank
  away_rankings <- away_rankings %>%
    group_by(OUTCOME) %>%
    arrange(OUTCOME, desc(PERCENTAGE)) %>%
    # Wins and draws are ranked best to worst, with rankings being reversed for losses (i.e. the team
    # with the fewest losses is ranked first, rather than last)
    mutate(RANK = case_when(OUTCOME %in% c("WIN", "DRAW") ~ rank(-PERCENTAGE, ties.method = "min"),
                            OUTCOME %in% c("LOSS") ~ rank(PERCENTAGE, ties.method = "min")))
  
  return(away_rankings)
}