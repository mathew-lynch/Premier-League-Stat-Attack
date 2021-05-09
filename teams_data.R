library(rvest)
library(stringi)
library(stringr)
library(tidyverse)
library(jsonlite)

# Code sourced from ewenme's understatr package on GitHub (https://github.com/ewenme/understatr)

get_team_data <- function() {
  
  # Read HTML from understat.com
  page <- session("https://understat.com/league/EPL")
  
  # By inspecting the source code, we note that the team xG data is contained in a script
  teams_data <- as.character(html_nodes(page, "script"))
  
  # Unescapes all known escape sequences
  get_data_element <- function(x, element_name) {
    stri_unescape_unicode(str_subset(x, element_name))
  }
  
  # Sources script referring to team data and unescapes all known escape sequences
  teams_data <- get_data_element(teams_data, "teamsData")
  
  # Pick out JSON string
  teams_data <- sub(".*?\\'(.*)\\'.*", "\\1", teams_data)
  
  # Use jsonlite package to convert JSON to dataframe
  teams_data <- fromJSON(teams_data, simplifyDataFrame = TRUE,
                         flatten = TRUE)
  
  # Get team data
  teams_data <- lapply(
    teams_data, function(x) {
      df <- x$history
      df$team_id <- x$id
      df$team_name <- x$title
      df
    })
  
  # Convert to dataframe
  teams_df <- do.call("rbind", teams_data)
  
  # Fix date column
  teams_df$date <- as.Date(teams_df$date, "%Y-%m-%d")
  
  # Convert to tibble
  teams_df <- as_tibble(teams_df)
 
  # Create a 'league' table
  overall_df <- tibble(
    Team = character(0),
    Matches = numeric(0),
    Wins = numeric(0),
    Draws = numeric(0),
    Losses = numeric(0),
    Goals = numeric(0),
    GA = numeric(0),
    Points = numeric(0),
    xG = numeric(0),
    xGA = numeric(0),
    xPTS = numeric(0)
  )
  
  # Populate league table with data from teams_df
  team_names <- unique(teams_df$team_name)
  for (i in 1:length(team_names)) {
    team_data = teams_df %>% filter(team_name == team_names[i])
    team_name = team_names[i]
    matches = nrow(team_data)
    wins = sum(team_data$wins)
    draws = sum(team_data$draws)
    losses = sum(team_data$loses)
    goals = sum(team_data$scored)
    GA = sum(team_data$missed)
    points = sum(team_data$pts)
    xg = sum(team_data$xG)
    xga = sum(team_data$xGA)
    xpts = sum(team_data$xpts)
    
    # Add data to tibble
    overall_df <- overall_df %>% add_row(Team = team_name, Matches = matches, 
                                        Wins = wins, Draws = draws, Losses = losses,
                                        Goals = goals, GA = GA, Points = points,
                                        xG = xg, xGA = xga, xPTS = xpts)
  }
  
  # Reorder by number of points
  overall_df <- arrange(overall_df, -Points)
  
  return(overall_df)
}