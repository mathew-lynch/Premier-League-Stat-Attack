library(rvest)
library(stringi)
library(stringr)
library(tidyverse)
library(qdapRegex)
library(jsonlite)

# Code sourced from ewenme's understatr package on GitHub (https://github.com/ewenme/understatr)

get_player_data <- function() {
  # Read HTML from understat.com
  page <- session("https://understat.com/league/EPL")
  
  # By inspecting the source code, we note that the player xG data is contained in a script
  players_data <- as.character(html_nodes(page, "script"))
  
  # Unescapes all known escape sequences
  get_data_element <- function(x, element_name) {
    stri_unescape_unicode(str_subset(x, element_name))
  }
  
  # Sources script referring to player data and unescapes all known escape sequences
  players_data <- get_data_element(players_data, "playersData")
  
  # Remove square brackets from JSON string
  fix_json <- function(x) {
    str_subset(
      unlist(
        rm_square(
          x, extract = TRUE, include.markers = TRUE
        )
      ),
      "\\[\\]", negate = TRUE
    )
  }
  
  players_data <- fix_json(players_data)
  
  # Use jsonlite package to convert JSON to dataframe
  players_data <- fromJSON(players_data)
  
  # Convert character to numerical
  is_all_numeric <- function(x) {
    !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
  }
  players_data <- players_data %>% 
    mutate_if(is_all_numeric,as.numeric)
  
  # Add new columns to data
  players_data$xG90 = (players_data$xG/players_data$time)*90
  players_data$npxG90 = (players_data$npxG/players_data$time)*90
  players_data$xA90 = (players_data$xA/players_data$time)*90
  players_data$xG90_plus_xA90 = players_data$xG90 + players_data$xA90
  players_data$npxG90_plus_xA90 = players_data$npxG90 + players_data$xA90
  players_data$xgChain90 = (players_data$xGChain/players_data$time)*90
  players_data$xgBuildup90 = (players_data$xGBuildup/players_data$time)*90
  
  # Add goal and assist rank to data
  players_data <- players_data %>%
    mutate(goal_rank = rank(-goals, ties.method = "min"))
  
  players_data <- players_data %>%
    mutate(xG_rank = rank(-xG, ties.method = "min"))
  
  players_data <- players_data %>%
    mutate(assist_rank = rank(-assists, ties.method = "min"))
  
  players_data <- players_data %>%
    mutate(xA_rank = rank(-xA, ties.method = "min"))
  
  
  # Convert to tibble
  players_df <- as_tibble(players_data)
  
  return(players_df)
}