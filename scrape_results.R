# Scrapes Premier League results for every team from https://www.espn.co.uk and returns a table of results
# URL is https://www.espn.co.uk/football/team/results/_/id/team_id/league/ENG.1/season/2022 where team_id
# is one of the entries in the vector team_ids

library(textreadr)
library(rvest)

team_names = c("AFC Bournemouth", "Arsenal", "Aston Villa", "Brentford", "Brighton & Hove Albion", "Chelsea",
               "Crystal Palace", "Everton", "Fulham", "Leeds United", "Leicester City",
               "Liverpool", "Manchester City", "Manchester United", "Newcastle United",
               "Nottingham Forest", "Southampton", "Tottenham Hotspur",
               "West Ham United", "Wolverhampton Wanderers")

team_ids = c("349", "359", "362", "337", "331", "363", "384", "368", "370", "357", "375", "364",
             "382", "360", "361", "393", "376", "367", "371", "380")

scrape_results <- function() {
  # Initialise empty data frame
  full_results <- data.frame()
  
  for (i in 1:length(team_ids)) {
    # Scrape Premier League data from ESPN
    url <- paste("https://www.espn.co.uk/football/team/results/_/id/", team_ids[i], "/league/ENG.1/season/2022", sep = "")
    webpage <- session(url)
    results_node <- html_nodes(webpage, "table")
    results <- html_table(results_node, fill=TRUE, header=TRUE)
    results <- do.call(rbind,results)
    results$TEAM_NAME <- rep(team_names[i], nrow(results))
    full_results <- rbind(full_results, results)
  }
  
  # Rename columns
  names(full_results) <- c("DATE", "HOME", "SCORE", "AWAY", "RESULT", "COMPETITION", "TEAM_NAME")
  
  return(full_results)
}