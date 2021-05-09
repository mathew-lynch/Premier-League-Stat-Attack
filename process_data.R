# Take the scraped results and calculate home/away win/draw/loss totals and percentages. Function requires a
# tibble containing the results, and the team name

process_data <- function(raw_results, t_name) {
  
  # Rename columns to make them unique
  colnames(raw_results) <- c("DATE", "HOME", "SCORE", "AWAY", "RESULT", "COMPETITION", "TEAM NAME")
  
  # Drop the RESULT, COMPETITION and TEAM NAME columns as these aren't required in the calculations
  to_drop <- c("RESULT", "COMPETITION", "TEAM NAME")
  raw_results <- raw_results %>% select(-one_of(to_drop))

  # Generate a vector of number of goals scored by home and away team in each game
  goals <- unlist(strsplit(raw_results$SCORE, split = " - "))

  # Divide home and away goals into two columns
  home <- vector()
  away <- vector()

  for (i in 1:length(goals)) {
    if ((i %% 2) == 0) {
      # Index is even - corresponds to away goals
      away <- append(away, goals[i])
    } else {
      # Index is odd - corresponds to home goals
      home <- append(home, goals[i])
    }
  }

  # Add this information to the dataframe
  raw_results$HOME_GOALS <- home
  raw_results$AWAY_GOALS <- away

  # Add a column to determine if chosen team are home or away
  home_or_away <- vector()

  for (j in 1:nrow(raw_results)) {
    if (t_name %in% raw_results$HOME[j]) {
      home_or_away <- append(home_or_away, "HOME")
    } else {
      home_or_away <- append(home_or_away, "AWAY")
    }
  }

  # Add this information to the dataframe
  raw_results$HOME_OR_AWAY <- home_or_away

  # Determine the winning team (or if the match was a draw)
  outcome <- vector()

  for (k in 1:nrow(raw_results)) {
    # Home win
    if (raw_results$HOME_GOALS[k] > raw_results$AWAY_GOALS[k]) {
      outcome <- append(outcome, "HOME")
      # Away win  
    } else if (raw_results$AWAY_GOALS[k] > raw_results$HOME_GOALS[k]) {
      outcome <- append(outcome, "AWAY")
      # Only other possible outcome is a draw
    } else {
      outcome <- append(outcome, "DRAW")
    }
  }

  # Add this information to the dataframe
  raw_results$OUTCOME <- outcome
  
  # Add a column which determines win, draw or loss
  result <- vector()
  
  for (m in 1:nrow(raw_results)) {
    # Win
    if (raw_results$HOME_OR_AWAY[m] == raw_results$OUTCOME[m]) {
      result <- append(result, "1")
    } else if (raw_results$OUTCOME[m] == "DRAW") {
      result <- append(result, "0")
    } else {
      result <- append(result, "-1")
    }
  }
  
  # Add this information to the dataframe
  raw_results$RESULT <- result
  
  return(raw_results)
}