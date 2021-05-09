# Home matches - calculate proportions of wins/draws/losses

home_wins <- function(results) {
  home <- results[results$HOME_OR_AWAY == "HOME",]
  home_wins <- home[home$OUTCOME == "HOME",]
  percent_home_wins <- nrow(home_wins)/nrow(home)
  return(percent_home_wins)
}

home_draws <- function(results) {
  home <- results[results$HOME_OR_AWAY == "HOME",]
  home_draws <- home[home$OUTCOME == "DRAW",]
  percent_home_draws <- nrow(home_draws)/nrow(home)
  return(percent_home_draws)
}

home_losses <- function(results) {
  home <- results[results$HOME_OR_AWAY == "HOME",]
  home_losses <- home[home$OUTCOME == "AWAY",]
  percent_home_losses <- nrow(home_losses)/nrow(home)
  return(percent_home_losses)
}

# Away matches - calculate proportions of wins/draws/losses

away_wins <- function(results) {
  away <- results[results$HOME_OR_AWAY == "AWAY",]
  away_wins <- away[away$OUTCOME == "AWAY",]
  percent_away_wins <- nrow(away_wins)/nrow(away)
  return(percent_away_wins)
}

away_draws <- function(results) {
  away <- results[results$HOME_OR_AWAY == "AWAY",]
  away_draws <- away[away$OUTCOME == "DRAW",]
  percent_away_draws <- nrow(away_draws)/nrow(away)
  return(percent_away_draws)
}

away_losses <- function(results) {
  away <- results[results$HOME_OR_AWAY == "AWAY",]
  away_losses <- away[away$OUTCOME == "HOME",]
  percent_away_losses <- nrow(away_losses)/nrow(away)
  return(percent_away_losses)
}