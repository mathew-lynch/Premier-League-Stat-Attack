# Load modules required for analysis

library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyverse)
library(rvest)
library(ECharts2Shiny)
library(DT)
library(ggplot2)
library(ggrepel)
library(scales)
library(billboarder)
library(shinyWidgets)
library(shinyjs)
library(pracma)
library(RColorBrewer)
library(fmsb)

################################################################################

# Code called before app launch

# Team names - these are different for different data sources (i.e. ESPN and Understat)
results_team_names = c("Arsenal", "Aston Villa", "Brighton & Hove Albion", "Burnley", "Chelsea",
                       "Crystal Palace", "Everton", "Fulham", "Leeds United", "Leicester City",
                       "Liverpool", "Manchester City", "Manchester United", "Newcastle United",
                       "Sheffield United", "Southampton", "Tottenham Hotspur", "West Bromwich Albion",
                       "West Ham United", "Wolverhampton Wanderers")

xG_team_names = c("Arsenal", "Aston Villa", "Brighton", "Burnley", "Chelsea",
                  "Crystal Palace", "Everton", "Fulham", "Leeds", "Leicester",
                  "Liverpool", "Manchester City", "Manchester United", "Newcastle United",
                  "Sheffield United", "Southampton", "Tottenham", "West Bromwich Albion",
                  "West Ham", "Wolverhampton Wanderers")

# 1. Results analysis

# Scrape results for every Premier league team
source("scrape_results.R")
full_results = scrape_results()

# Calculate home/away win/draw/loss totals and percentages for a PARTICULAR team
source("process_data.R")
source("home_and_away.R")

# Calculate win/draw/loss totals and percentages for home and away games for EVERY team
source("summary_all.R")
processed_results = summary_all(full_results, team_names)

# Calculate home and away rankings
source("home_and_away_rankings.R")
home_rankings <- home_rankings(processed_results, team_names)
away_rankings <- away_rankings(processed_results, team_names)


# 2. Team xG

# Scrape team xG data
source("teams_data.R")
team_xG <- get_team_data()

# Calculate differences between goals/points and expected goals/points
team_xG <- team_xG %>% 
  add_column(Goals_minus_xG = team_xG$Goals - team_xG$xG,
             GA_minus_xGA = team_xG$GA - team_xG$xGA,
             Points_minus_xPTS = team_xG$Points - team_xG$xPTS)

# Create a league ordered by xPTS (expected points)
league_by_xpts <- team_xG %>%
  select(Team, Matches, Wins, Draws, Losses, Goals, GA, Points, xG, xGA, xPTS) %>%
  arrange(-xPTS)


# 3. Player xG

# Scrape player xG data
source("players_data.R")
player_xG <- get_player_data()

# Apostrophes have been encoded strangely - this is corrected
player_xG$player_name <- gsub("&#039;", "'", player_xG$player_name)


################################################################################

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Premier League Stat Attack", titleWidth = 400),
  dashboardSidebar(
    width = 110,
    sidebarMenu(id = "tabs",
      menuItem("Results", tabName = "results", icon = icon("futbol")),
      menuItem("Team xG", tabName = "team_xG", icon = icon("users")),
      menuItem("Player xG", tabName = "player_xG", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Results
      tabItem(tabName = "results",
              # Colour of the header - will change depending on the team selected
              uiOutput("results_colour"),
              
              fluidRow(
                # Select team
                column(3,
                       style='padding-left:20px; padding-right:0px; padding-top:40px; padding-bottom:40px',
                       selectInput(inputId = "results_team", label = "Select team:",
                                   choices = results_team_names, selected = "Arsenal")),
                
                # Number of games played
                column(2,
                       align = "center",
                       valueBoxOutput("played_box", width = 8)),
                
                # Number of games won
                column(2,
                       align = "center",
                       valueBoxOutput("won_box", width = 8)),
                
                # Number of games drawn
                column(2,
                       align = "center",
                       valueBoxOutput("drawn_box", width = 8)),
                
                # Number of games lost
                column(2,
                       align = "center",
                       valueBoxOutput("lost_box", width = 8)),
              ),
              
              fluidRow(
                
                # Credit ESPN
                column(
                       h5(paste("Data from ESPN.co.uk Premier League results: espn.co.uk/football/ accessed on", format(Sys.Date(), "%d-%m-%Y"))),
                       actionButton("do", "Visit ESPN.co.uk",
                                 icon = icon("futbol"),
                                 onclick ="window.open('https://www.espn.co.uk/football/', '_blank')"),
                       width=12,style="background-color:lavender;border-radius: 10px;height: 75px")
              ),
              
              fluidRow(
                
                # Fixtures
                column(6,
                       align = "center",
                       wellPanel(DT::dataTableOutput("results"), style = "height:292px; overflow-y: scroll;overflow-x: scroll;")),
                
                # Boxes containing home and away performances  
                column(6,
                       align = "center",
                       tags$head(tags$style(HTML(".small-box {height: 136px}"))),
                       valueBoxOutput("home_win_percentage_box"),
                       valueBoxOutput("home_draw_percentage_box"),
                       valueBoxOutput("home_loss_percentage_box"),
                       valueBoxOutput("away_win_percentage_box"),
                       valueBoxOutput("away_draw_percentage_box"),
                       valueBoxOutput("away_loss_percentage_box"))
              ),
              
              fluidRow(
                
                # Explanation of home results in table (win, draw, loss)
                column(3,
                       align = "center",
                       htmlOutput("home_type")),
                
                # Instructions on how to display results
                column(6,
                       align = "center",
                       htmlOutput("pie_instructions")),
                
                # Explanation of away results in table (win, draw, loss)
                column(3,
                       align = "center",
                       htmlOutput("away_type"))
                
              ),
              
              fluidRow(
                
                # Home games by win, draw or loss
                column(3,
                       align = "center",
                       wellPanel(DT::dataTableOutput("home_click"), style = "font-size:80%; height:292px; overflow-y: scroll;overflow-x: scroll;")),
                
                # Home games pie chart
                column(3,
                       align = "right",
                       tags$div(id="pie_home", style="width:100%;height:225px"),
                       billboarderOutput(outputId = "pie_home", height = 225)),
                
                # Away games pie chart
                column(3,
                       align = "left",
                       tags$div(id="pie_away", style="width:100%;height:225px"),
                       billboarderOutput(outputId = "pie_away", height = 225)),
                
                # Away games by win, draw or loss
                column(3,
                       align = "center",
                       wellPanel(DT::dataTableOutput("away_click"), style = "font-size:80%; height:292px; overflow-y: scroll;overflow-x: scroll;"))
              )
      ),
      
      # Team xG
      tabItem(tabName = "team_xG",
              uiOutput("teams_colour"),
              useShinyalert(),
              fluidRow(
                column(
                  h4(paste("Data from understat.com Expected goals (xG) data: understat.com/league/EPL accessed on", format(Sys.Date(), "%d-%m-%Y"))),
                  actionButton("do", "Visit understat.com",
                               icon = icon("futbol"),
                               onclick ="window.open('https://understat.com/', '_blank')"),
                  width=12,style="background-color:lavender;border-radius: 10px;height: 75px")
              ),
              
              fluidRow(
                column(6,
                       align = "left",
                       "What if the league table was decided by xPTS (expected points)?"),
                
                column(6,
                       align = "left",
                       "Which teams are overperforming and underperforming their xG?")
              ),
              
              fluidRow(
                column(6,
                       align = "left",
                       DT::dataTableOutput("xPTS_table"), style = "font-size:70%; overflow-y: scroll;overflow-x: scroll;"),
                
                column(6,
                       align = "left",
                       plotOutput("teams_xG"))
              ),
              
              fluidRow(
                column(6,
                       align = "left",
                       "Comparing xG with xGA"),
              ),
              
              fluidRow(
                column(6,
                       align = "left",
                       plotOutput("xG_vs_xGA")),
                
                column(
                  h4("Select two teams to compare:"),
                  selectizeInput("team_one", label = "Team One", 
                                 selected = "Arsenal", multiple = FALSE,
                                 choices = xG_team_names),
                  selectizeInput("team_two", label = "Team Two", 
                                 selected = "Aston Villa", multiple = FALSE,
                                 choices = xG_team_names),
                  p("Explanation of terms:", br(), br(
                    "1. xG = Expected goals for", br(), br(
                      "2. xGA = Expected goals against", br(), br(
                        "3. xPTS = Expected points"))),
                    style="color:black;text-align:left"),
                  width=2,style="background-color:lavender;border-radius: 10px;height: 350px"),
                
                column(4,
                       align = "left",
                       plotOutput("team_comparison")))
              ),
      
      # Player xG
      tabItem(tabName = "player_xG",
              # Colour of the header - will change depending on the team selected
              uiOutput("players_colour"),
              
              fluidRow(
                column(3,
                       align = "left",
                       selectInput(inputId = "xG_team", label = "Select team:",
                                   choices = c("All", xG_team_names), selected = "All",
                                   multiple = F)),
              ),
              
              fluidRow(
                column(
                  h3(paste("Data from understat.com Expected goals (xG) data: understat.com/league/EPL accessed on", format(Sys.Date(), "%d-%m-%Y"))),
                  p("Explanation of terms:", br(), br(
                    "1. Delta is the difference between a player's expected and actual output;
               a negative delta indicates underperformance, whereas a positive delta indicates
               overperformance.", br(), br(
                 "2. xG90/xA90 = Expected goals/assists per 90 minutes", br(), br(
                   "3. npxG90 = Non-penalty expected goals per 90 minutes", br(), br(
                     "4. xgChain90 = Total xG of every possession the player is involved in per 90 minutes", br(), br(
                       "5. xgBuildup90 = Total xG of every possession the player is involved in without key passes and shots per 90 minutes"
                     )
                   )
                 )
               )
                  ),
               style="color:black;text-align:left"),
               width=5,style="background-color:lavender;border-radius: 10px;height: 350px"),
               
               column(2,
                      h4("Select two players to compare:"),
                      p(br()),
                      selectizeInput("player_one", label = "Player One", 
                                     selected = "Harry Kane", multiple = FALSE,
                                     choices = NULL),
                      selectizeInput("player_two", label = "Player Two", 
                                     selected = "Mohamed Salah", multiple = FALSE,
                                     choices = NULL),
                      p(br()),
                      actionButton("do", "Visit understat.com",
                                   icon = icon("futbol"),
                                   onclick ="window.open('https://understat.com/', '_blank')"),
                      style="background-color:lavender; height: 350px"),
               
               column(5,
                      plotOutput("spider_compare"),
                      style = "height: 375px")
              ),
              
              fluidRow(
                column(6,
                       align = "left",
                       "What if players were ranked by xG and xA?"),
                
                column(6,
                       align = "left",
                       "Which players are overperforming and underperforming their xG/xA?")
                
              ),
              
              fluidRow(
                column(6,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Rank by xG", DT::dataTableOutput("xG_rank"), style = "font-size:70%; height:300px; overflow-y: scroll;overflow-x: scroll;"),
                                   tabPanel("Rank by xA", DT::dataTableOutput("xA_rank"), style = "font-size:70%; height:300px; overflow-y: scroll;overflow-x: scroll;")
                       )),
                
                column(6,
                       tabsetPanel(type = "tabs",
                                   tabPanel("xG", plotOutput("xG_delta"), style = "font-size:70%"),
                                   tabPanel("xA", plotOutput("xA_delta"), style = "font-size:70%")
                       ))
              ),
              
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              )
      )
    )
  )
)

################################################################################

# SERVER

server <- function(input, output, session) {
  # 1. Results analysis
  
  # Get results corresponding to chosen team
  full_results_subset <- reactive({
    a <- subset(full_results, TEAM_NAME == input$results_team)
    return(a)
  })
  
  # Process results of chosen team
  results <- reactive({process_data(full_results_subset(), input$results_team)})
  
  # Home/away win/draw/loss percentages for chosen team
  percent_home_wins <- reactive({home_wins(results())})
  percent_home_draws <- reactive({home_draws(results())})
  percent_home_losses <- reactive({home_losses(results())})
  percent_away_wins <- reactive({away_wins(results())})
  percent_away_draws <- reactive({away_draws(results())})
  percent_away_losses <- reactive({away_losses(results())})
  
  # Rank in league
  home_win_ranking <- reactive({home_rankings$RANK[home_rankings$TEAM == input$results_team & home_rankings$OUTCOME == "WIN"]})
  home_draw_ranking <- reactive({home_rankings$RANK[home_rankings$TEAM == input$results_team & home_rankings$OUTCOME == "DRAW"]})
  home_loss_ranking <- reactive({home_rankings$RANK[home_rankings$TEAM == input$results_team & home_rankings$OUTCOME == "LOSS"]})
  away_win_ranking <- reactive({away_rankings$RANK[away_rankings$TEAM == input$results_team & away_rankings$OUTCOME == "WIN"]})
  away_draw_ranking <- reactive({away_rankings$RANK[away_rankings$TEAM == input$results_team & away_rankings$OUTCOME == "DRAW"]})
  away_loss_ranking <- reactive({away_rankings$RANK[away_rankings$TEAM == input$results_team & away_rankings$OUTCOME == "LOSS"]})
  
  # Slices for home games pie chart
  home_slices = reactive({
    data.frame("name" = c("Wins", "Draws", "Losses"), "value" = c(percent_home_wins(), percent_home_draws(), percent_home_losses()))
  })
  
  # Slices for away games pie chart
  away_slices = reactive({
    data.frame("name" = c("Wins", "Draws", "Losses"), "value" = c(percent_away_wins(), percent_away_draws(), percent_away_losses()))
  })
  
  # Results table
  output$results <- renderDataTable(select(full_results_subset(), -TEAM_NAME))
  
  # Home win percentage
  output$home_win_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_home_wins()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Home Win %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", home_win_ranking()), style = "font-size: 135%;")),
      color = "green"
    )
  })
  
  # Home draw percentage
  output$home_draw_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_home_draws()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Home Draw %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", home_draw_ranking()), style = "font-size: 135%;")),
      color = "teal"
    )
  })
  
  # Home loss percentage
  output$home_loss_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_home_losses()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Home Loss %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", home_loss_ranking()), style = "font-size: 135%;")),
      color = "red"
    )
  })
  
  # Away win percentage
  output$away_win_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_away_wins()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Away Win %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", away_win_ranking()), style = "font-size: 135%;")),
      color = "green"
    )
  })
  
  # Away draw percentage
  output$away_draw_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_away_draws()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Away Draw %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", away_draw_ranking()), style = "font-size: 135%;")),
      color = "teal"
    )
  })
  
  # Away loss percentage
  output$away_loss_percentage_box <- renderValueBox({
    valueBox(
      value = tags$p(label_percent()(percent_away_losses()), style = "font-size: 125%;"),
      subtitle = tags$div(
        tags$p("Away Loss %", style = "font-size: 135%;"),
        tags$p(paste("League rank: ", away_loss_ranking()), style = "font-size: 135%;")),
      color = "red"
    )
  })
  
  # Games played
  output$played_box <- renderValueBox({
    valueBox(
      value = tags$p(nrow(results()), style = "font-size: 175%;"),
      subtitle = tags$p("Played", style = "font-size: 200%;"),
      color = "black"
    )
  })
  
  # Games won
  output$won_box <- renderValueBox({
    valueBox(
      value = tags$p(length(results()$RESULT[results()$RESULT == "1"]), style = "font-size: 175%;"),
      subtitle = tags$p("Won", style = "font-size: 200%;"),
      color = "green"
    )
  })
  
  # Games drawn
  output$drawn_box <- renderValueBox({
    valueBox(
      value = tags$p(length(results()$RESULT[results()$RESULT == "0"]), style = "font-size: 175%;"),
      subtitle = tags$p("Drawn", style = "font-size: 200%;"),
      color = "teal"
    )
  })
  
  # Games lost
  output$lost_box <- renderValueBox({
    valueBox(
      value = tags$p(length(results()$RESULT[results()$RESULT == "-1"]), style = "font-size: 175%;"),
      subtitle = tags$p("Lost", style = "font-size: 200%;"),
      color = "red"
    )
  })
  
  # Custom colours for pie charts
  color_list <- list(Wins = "green", Draws = "teal", Losses = "red")
  
  # Home pie chart
  output$pie_home <- renderBillboarder({
    billboarder() %>%
      bb_piechart(home_slices()) %>%
      bb_title("Home", position = "top-center") %>%
      bb_pie(label = list(threshold = 0.1)) %>%
      bb_colors_manual(color_list) 
  })
  
  # Away pie chart
  output$pie_away <- renderBillboarder({
    billboarder() %>%
      bb_piechart(away_slices()) %>%
      bb_title("Away", position = "top-center") %>%
      bb_pie(label = list(threshold = 0.1)) %>%
      bb_colors_manual(color_list)
  })
  
  # Home games won - teams
  home_won_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "HOME" & 
                                processed_results$RESULT == "1",]
    return(data[,2:4])
  })
  
  # Home games drawn - teams
  home_drawn_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "HOME" & 
                                processed_results$RESULT == "0",]
    return(data[,2:4])
  })
  
  # Home games lost - teams
  home_lost_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "HOME" & 
                                processed_results$RESULT == "-1",]
    return(data[,2:4])
  })
  
  # Away games won - teams
  away_won_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "AWAY" & 
                                processed_results$RESULT == "1",]
    return(data[,2:4])
  })
  
  # Away games drawn - teams
  away_drawn_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "AWAY" & 
                                processed_results$RESULT == "0",]
    return(data[,2:4])
  })
  
  # Away games lost - teams
  away_lost_teams <- reactive({
    data <- processed_results[processed_results$TEAM_NAME == input$results_team & 
                                processed_results$HOME_OR_AWAY == "AWAY" & 
                                processed_results$RESULT == "-1",]
    return(data[,2:4])
  })
  
  
  # When home pie clicked - display results
  output$home_click <- renderDataTable({
    if (is.null(input$pie_home_click$id)) {
      tibble()
    }
    else if (input$pie_home_click$id == "Wins") {
      datatable(
        home_won_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
    else if (input$pie_home_click$id == "Draws") {
      datatable(
        home_drawn_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    } 
    else if (input$pie_home_click$id == "Losses") {
      datatable(
        home_lost_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # When home pie clicked - display explanatory text
  output$home_type <- renderText({
    if (is.null(input$pie_home_click$id)) {
      ""
    }
    else if (input$pie_home_click$id == "Wins") {
      "<b>Home Games Won<b>"
    }
    else if (input$pie_home_click$id == "Draws") {
      "<b>Home Games Drawn<b>"
    } 
    else if (input$pie_home_click$id == "Losses") {
      "<b>Home Games Lost<b>"
    }
  })
  
  
  # When away pie clicked - display results
  output$away_click <- renderDataTable({
    if (is.null(input$pie_away_click$id)) {
      tibble()
    }
    else if (input$pie_away_click$id == "Wins") {
      datatable(
        away_won_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
    else if (input$pie_away_click$id == "Draws") {
      datatable(
        away_drawn_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    } 
    else if (input$pie_away_click$id == "Losses") {
      datatable(
        away_lost_teams(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # When away pie clicked - display explanatory text
  output$away_type <- renderText({
    if (is.null(input$pie_away_click$id)) {
      ""
    }
    else if (input$pie_away_click$id == "Wins") {
      "<b>Away Games Won<b>"
    }
    else if (input$pie_away_click$id == "Draws") {
      "<b>Away Games Drawn<b>"
    } 
    else if (input$pie_away_click$id == "Losses") {
      "<b>Away Games Lost<b>"
    }
  })
  
  # Pie chart instructions
  output$pie_instructions <- renderText({
    "<em>Select a segment of the pie chart to reveal results<em>"
  })
  
  # Background colour - changes depending on team selected
  output$results_colour <- renderUI({
    # Ensures the colour only changes when on the correct tab
    if (input$tabs == "results") {
      if (input$results_team == "Arsenal") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EF0107;}", ".skin-blue .main-header .logo:hover {background-color:", "#EF0107;}", ".skin-blue .main-header .navbar {background-color:", "#EF0107;}")))))
      } 
      else if (input$results_team == "Aston Villa") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#670E36;}", ".skin-blue .main-header .logo:hover {background-color:", "#670E36;}", ".skin-blue .main-header .navbar {background-color:", "#670E36;}")))))
      } 
      else if (input$results_team == "Brighton & Hove Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#0057B8;}", ".skin-blue .main-header .logo:hover {background-color:", "#0057B8;}", ".skin-blue .main-header .navbar {background-color:", "#0057B8;}")))))
      } 
      else if (input$results_team == "Burnley") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6C1D45;}", ".skin-blue .main-header .logo:hover {background-color:", "#6C1D45;}", ".skin-blue .main-header .navbar {background-color:", "#6C1D45;}")))))
      } 
      else if (input$results_team == "Chelsea") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#034694;}", ".skin-blue .main-header .logo:hover {background-color:", "#034694;}", ".skin-blue .main-header .navbar {background-color:", "#034694;}")))))
      } 
      else if (input$results_team == "Crystal Palace") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#1B458F;}", ".skin-blue .main-header .logo:hover {background-color:", "#1B458F;}", ".skin-blue .main-header .navbar {background-color:", "#1B458F;}")))))
      }
      else if (input$results_team == "Everton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003399;}", ".skin-blue .main-header .logo:hover {background-color:", "#003399;}", ".skin-blue .main-header .navbar {background-color:", "#003399;}")))))
      }
      else if (input$results_team == "Fulham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#000000;}", ".skin-blue .main-header .logo:hover {background-color:", "#000000;}", ".skin-blue .main-header .navbar {background-color:", "#000000;}")))))
      }
      else if (input$results_team == "Leeds United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FFCD00;}", ".skin-blue .main-header .logo:hover {background-color:", "#FFCD00;}", ".skin-blue .main-header .navbar {background-color:", "#FFCD00;}")))))
      }
      else if (input$results_team == "Leicester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003090;}", ".skin-blue .main-header .logo:hover {background-color:", "#003090;}", ".skin-blue .main-header .navbar {background-color:", "#003090;}")))))
      }
      else if (input$results_team == "Liverpool") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#C8102E;}", ".skin-blue .main-header .logo:hover {background-color:", "#C8102E;}", ".skin-blue .main-header .navbar {background-color:", "#C8102E;}")))))
      }
      else if (input$results_team == "Manchester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6CABDD;}", ".skin-blue .main-header .logo:hover {background-color:", "#6CABDD;}", ".skin-blue .main-header .navbar {background-color:", "#6CABDD;}")))))
      }
      else if (input$results_team == "Manchester United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#DA291C;}", ".skin-blue .main-header .logo:hover {background-color:", "#DA291C;}", ".skin-blue .main-header .navbar {background-color:", "#DA291C;}")))))
      }
      else if (input$results_team == "Newcastle United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#241F20;}", ".skin-blue .main-header .logo:hover {background-color:", "#241F20;}", ".skin-blue .main-header .navbar {background-color:", "#241F20;}")))))
      }
      else if (input$results_team == "Sheffield United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EE2737;}", ".skin-blue .main-header .logo:hover {background-color:", "#EE2737;}", ".skin-blue .main-header .navbar {background-color:", "#EE2737;}")))))
      }
      else if (input$results_team == "Southampton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#D71920;}", ".skin-blue .main-header .logo:hover {background-color:", "#D71920;}", ".skin-blue .main-header .navbar {background-color:", "#D71920;}")))))
      }
      else if (input$results_team == "Tottenham Hotspur") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#132257;}", ".skin-blue .main-header .logo:hover {background-color:", "#132257;}", ".skin-blue .main-header .navbar {background-color:", "#132257;}")))))
      }
      else if (input$results_team == "West Bromwich Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#122F67;}", ".skin-blue .main-header .logo:hover {background-color:", "#122F67;}", ".skin-blue .main-header .navbar {background-color:", "#122F67;}")))))
      }
      else if (input$results_team == "West Ham United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#7A263A;}", ".skin-blue .main-header .logo:hover {background-color:", "#7A263A;}", ".skin-blue .main-header .navbar {background-color:", "#7A263A;}")))))
      }
      else if (input$results_team == "Wolverhampton Wanderers") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FDB913;}", ".skin-blue .main-header .logo:hover {background-color:", "#FDB913;}", ".skin-blue .main-header .navbar {background-color:", "#FDB913;}")))))
      }
    }
    else if (input$tabs == "player_xG") {
      if (input$xG_team == "All") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#928FA5;}", ".skin-blue .main-header .logo:hover {background-color:", "#928FA5;}", ".skin-blue .main-header .navbar {background-color:", "#928FA5;}")))))
      }
      else if (input$xG_team == "Arsenal") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EF0107;}", ".skin-blue .main-header .logo:hover {background-color:", "#EF0107;}", ".skin-blue .main-header .navbar {background-color:", "#EF0107;}")))))
      } 
      else if (input$xG_team == "Aston Villa") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#670E36;}", ".skin-blue .main-header .logo:hover {background-color:", "#670E36;}", ".skin-blue .main-header .navbar {background-color:", "#670E36;}")))))
      } 
      else if (input$xG_team == "Brighton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#0057B8;}", ".skin-blue .main-header .logo:hover {background-color:", "#0057B8;}", ".skin-blue .main-header .navbar {background-color:", "#0057B8;}")))))
      } 
      else if (input$xG_team == "Burnley") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6C1D45;}", ".skin-blue .main-header .logo:hover {background-color:", "#6C1D45;}", ".skin-blue .main-header .navbar {background-color:", "#6C1D45;}")))))
      } 
      else if (input$xG_team == "Chelsea") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#034694;}", ".skin-blue .main-header .logo:hover {background-color:", "#034694;}", ".skin-blue .main-header .navbar {background-color:", "#034694;}")))))
      } 
      else if (input$xG_team == "Crystal Palace") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#1B458F;}", ".skin-blue .main-header .logo:hover {background-color:", "#1B458F;}", ".skin-blue .main-header .navbar {background-color:", "#1B458F;}")))))
      }
      else if (input$xG_team == "Everton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003399;}", ".skin-blue .main-header .logo:hover {background-color:", "#003399;}", ".skin-blue .main-header .navbar {background-color:", "#003399;}")))))
      }
      else if (input$xG_team == "Fulham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#000000;}", ".skin-blue .main-header .logo:hover {background-color:", "#000000;}", ".skin-blue .main-header .navbar {background-color:", "#000000;}")))))
      }
      else if (input$xG_team == "Leeds") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FFCD00;}", ".skin-blue .main-header .logo:hover {background-color:", "#FFCD00;}", ".skin-blue .main-header .navbar {background-color:", "#FFCD00;}")))))
      }
      else if (input$xG_team == "Leicester") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003090;}", ".skin-blue .main-header .logo:hover {background-color:", "#003090;}", ".skin-blue .main-header .navbar {background-color:", "#003090;}")))))
      }
      else if (input$xG_team == "Liverpool") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#C8102E;}", ".skin-blue .main-header .logo:hover {background-color:", "#C8102E;}", ".skin-blue .main-header .navbar {background-color:", "#C8102E;}")))))
      }
      else if (input$xG_team == "Manchester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6CABDD;}", ".skin-blue .main-header .logo:hover {background-color:", "#6CABDD;}", ".skin-blue .main-header .navbar {background-color:", "#6CABDD;}")))))
      }
      else if (input$xG_team == "Manchester United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#DA291C;}", ".skin-blue .main-header .logo:hover {background-color:", "#DA291C;}", ".skin-blue .main-header .navbar {background-color:", "#DA291C;}")))))
      }
      else if (input$xG_team == "Newcastle United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#241F20;}", ".skin-blue .main-header .logo:hover {background-color:", "#241F20;}", ".skin-blue .main-header .navbar {background-color:", "#241F20;}")))))
      }
      else if (input$xG_team == "Sheffield United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EE2737;}", ".skin-blue .main-header .logo:hover {background-color:", "#EE2737;}", ".skin-blue .main-header .navbar {background-color:", "#EE2737;}")))))
      }
      else if (input$xG_team == "Southampton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#D71920;}", ".skin-blue .main-header .logo:hover {background-color:", "#D71920;}", ".skin-blue .main-header .navbar {background-color:", "#D71920;}")))))
      }
      else if (input$xG_team == "Tottenham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#132257;}", ".skin-blue .main-header .logo:hover {background-color:", "#132257;}", ".skin-blue .main-header .navbar {background-color:", "#132257;}")))))
      }
      else if (input$xG_team == "West Bromwich Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#122F67;}", ".skin-blue .main-header .logo:hover {background-color:", "#122F67;}", ".skin-blue .main-header .navbar {background-color:", "#122F67;}")))))
      }
      else if (input$xG_team == "West Ham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#7A263A;}", ".skin-blue .main-header .logo:hover {background-color:", "#7A263A;}", ".skin-blue .main-header .navbar {background-color:", "#7A263A;}")))))
      }
      else if (input$xG_team == "Wolverhampton Wanderers") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FDB913;}", ".skin-blue .main-header .logo:hover {background-color:", "#FDB913;}", ".skin-blue .main-header .navbar {background-color:", "#FDB913;}")))))
      }
    }
    else if (input$tabs == "team_xG") {
      return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#928FA5;}", ".skin-blue .main-header .logo:hover {background-color:", "#928FA5;}", ".skin-blue .main-header .navbar {background-color:", "#928FA5;}")))))
    }
  })
  
  
  # 2. Team xG
  output$xPTS_table <- renderDataTable({
    DT::datatable(league_by_xpts)
  })
  
  output$teams_xG <- renderPlot({
    ggplot(team_xG, aes(x=Goals, y=xG)) + 
      geom_point() +
      geom_abline(intercept = 0) + 
      geom_text_repel(aes(label=Team)) +
      annotate("text", x = max(team_xG$Goals), y = min(team_xG$xG), label = "Overperforming", color = "dark green") +
      annotate("text", x = min(team_xG$Goals), y = max(team_xG$xG), label = "Underperforming", color = "red") +
      xlim(min(team_xG$Goals)-5, max(team_xG$Goals)+5) +
      ylim(min(team_xG$xG)-5, max(team_xG$xG)+5)
  })
  
  output$xG_vs_xGA <- renderPlot({
    ggplot(team_xG, aes(x=xG, y=xGA)) +
      geom_point() +
      geom_text_repel(aes(label=Team)) +
      geom_hline(yintercept = mean(team_xG$xGA)) +
      geom_vline(xintercept = mean(team_xG$xG)) + 
      annotate("text", x = min(team_xG$xG)-10, y = min(team_xG$xGA)-10, parse=T, label = '"Good defence, " * phantom("poor attack")', color = "dark green") +
      annotate("text", x = min(team_xG$xG)-10, y = min(team_xG$xGA)-10, parse=T, label = 'phantom("Good defence, ") * "poor attack"', color = "red") +
      annotate("text", x = max(team_xG$xG)+10, y = min(team_xG$xGA)-10, label = "Good defence, good attack", color = "dark green") +
      annotate("text", x = min(team_xG$xG)-10, y = max(team_xG$xGA)+10, label = "Poor defence, poor attack", color = "red") +
      annotate("text", x = max(team_xG$xG)+10, y = max(team_xG$xGA)+10, parse=T, label = '"Poor defence, " * phantom("good attack")', color = "red") +
      annotate("text", x = max(team_xG$xG)+10, y = max(team_xG$xGA)+10, parse=T, label = 'phantom("Poor defence, ") * "good attack"', color = "dark green") +
      xlim(min(team_xG$xG)-15, max(team_xG$xG)+15) +
      ylim(min(team_xG$xGA)-15, max(team_xG$xGA)+15)
  })
  
  # Filter out two teams to compare
  teams_to_compare <- reactive({c(input$team_one, input$team_two)})
  
  # Throw error if the user selects the same team twice
  observe({
    if(input$team_one == input$team_two)
      shinyalert("Oops!", "Select two different teams to compare", type = "error")
  })
  
  # Filter data
  team_plot_data <- reactive({
    team_xG %>%
      filter(Team %in% teams_to_compare()) %>%
      select(Team, xG, xGA, xPTS) %>%
      gather(Metric, Value, xG:xPTS, factor_key = TRUE) %>%
      mutate(Value = if_else(Team == input$team_two, -Value, Value))
  })
  
  # Find the order
  temp_df <- reactive({
    team_plot_data() %>%
      filter(Team == input$team_one) %>%
      arrange(Value)
  })
  
  the_order <- reactive({
    temp_df()$Metric
  })
  
  # Plot back-to-back bar chart
  output$team_comparison <- renderPlot({
    if (input$team_one == input$team_two) {
      plot.new()
    } else {
      ggplot(team_plot_data(), aes(x = Metric, y = Value, group = Team, fill = Team)) +
        geom_bar(stat = "identity", width = 0.75) +
        coord_flip() +
        labs(x = "Metric", y = "Value", title = paste(teams_to_compare()[1], "vs.", teams_to_compare()[2])) +
        geom_text(aes(label=abs(round(Value,2))), hjust = "inward") + 
        theme(
          axis.text.x = element_blank(),
          axis.ticks = element_blank())
    }
  })
  
  
  # 3. Player xG
  # Get data corresponding to chosen team
  player_data <- reactive({
    if (input$xG_team == "All") {
      return(player_xG)
    } else {
      player_xG <- player_xG %>%
        separate(team_title, c("team_title", "team_title_two"), ",")
      
      player_data <- player_xG %>%
        filter(team_title == input$xG_team | team_title_two == input$xG_team)
      
      return(player_data)
    }
  })
  
  # Update players that can be selected based on team selected
  observe({
    updateSelectizeInput(session, 'player_one', selected = player_data()$player_name[1], choices = list(unique(player_data()$player_name)), server = TRUE)
    updateSelectizeInput(session, 'player_two', selected = player_data()$player_name[2], choices = list(unique(player_data()$player_name)), server = TRUE)
  })
  
  # Top scorers based on xG
  top_xG <- reactive({
    player_data() %>%
      arrange(-xG) %>%
      mutate(xG = round(xG, 2)) %>%
      mutate(Delta = round(goals - xG, 2)) %>%
      select(player_name, team_title, goals, xG, Delta) %>%
      rename(Name = player_name,
             Team = team_title,
             Goals = goals)
  })
  
  # Top assisters based on xA
  top_xA <- reactive({
    player_data() %>%
      arrange(-xA) %>%
      mutate(xA = round(xA, 2)) %>%
      mutate(Delta = round(assists - xA, 2)) %>%
      select(player_name, team_title, assists, xA, Delta) %>%
      rename(Name = player_name,
             Team = team_title,
             Assists = assists)
  })
  
  # Players ranked by xG
  output$xG_rank <- renderDataTable({
    DT::datatable(top_xG()) %>%
      formatStyle(
        "Delta",
        color = styleInterval(0, c("red", "green"))
      )
  })
  
  # Players ranked by xA
  output$xA_rank <- renderDataTable({
    DT::datatable(top_xA()) %>%
      formatStyle(
        "Delta",
        color = styleInterval(0, c("red", "green"))
      )
  })
  
  # Get top 10 overperforming and underperforming players
  top_10_over_xG = reactive({
    top_xG() %>%
      top_n(10, Delta)
  })
  
  top_10_under_xG = reactive({
    top_xG() %>%
      top_n(10, -Delta)
  })
  
  top_10_over_xA = reactive({
    top_xA() %>%
      top_n(10, Delta)
  })
  
  top_10_under_xA = reactive({
    top_xA() %>%
      top_n(10, -Delta)
  })
  
  # Bind tibbles
  top_10_xG = reactive({
    rbind(top_10_under_xG(), top_10_over_xG()) %>%
      unique() %>%
      mutate(Performance = ifelse(Delta < 0, "Underperforming", "Overperforming")) %>%
      mutate(Colour = ifelse(Performance == "Underperforming", "green", "red"))
  })
  
  top_10_xA = reactive({
    rbind(top_10_over_xA(), top_10_under_xA()) %>%
      unique() %>%
      mutate(Performance = ifelse(Delta < 0, "Underperforming", "Overperforming")) %>%
      mutate(Colour = ifelse(Performance == "Underperforming", "green", "red"))
  })
  
  # Delta xG bar chart - deltaxG = Goals - xG
  output$xG_delta <- renderPlot({
    ggplot(top_10_xG(), aes(x = reorder(Name, Delta), y = Delta, fill = Colour)) +
      geom_col() +
      coord_flip() +
      labs(x = "Player", y = "xG Delta (Goals - xG)") +
      theme(legend.position = "none")
  }, height = 300)
  
  # Delta xA bar chart - deltaxA = Assists - xA
  output$xA_delta <- renderPlot({
    ggplot(top_10_xA(), aes(x = reorder(Name, Delta), y = Delta, fill = Colour)) +
      geom_col() +
      coord_flip() +
      labs(x = "Player", y = "xA Delta (Assists - xA)") +
      theme(legend.position = "none")
  }, height = 300)
  
  # Spider/radar chart to compare two players
  spider_data = reactive({
    player_data() %>%
      select(player_name, xG90, xA90, npxG90, xgChain90, xgBuildup90) %>%
      column_to_rownames('player_name')
  })
  
  # Filter out two players to compare
  players_to_compare <- reactive({c(input$player_one, input$player_two)})
  
  player_plot_data <- reactive({
    spider_data() %>%
      filter(rownames(spider_data()) %in% players_to_compare())
  })
  
  # Add the maximum, minimum row value
  final_data <- reactive({
    rbind(rep(max(player_plot_data()), 5), rep(0, 5), player_plot_data())
  })
  
  # Define fill colours
  colours_fill <- c(scales::alpha("red", 0.1),
                    scales::alpha("blue", 0.1))
  colours_line <- c(scales::alpha("darkred", 0.9),
                    scales::alpha("darkblue", 0.9))
  
  # Plot radarchart
  output$spider_compare <- renderPlot({
    radarchart(final_data(),
               seg = 5,
               title = paste(players_to_compare()[1], "vs.", players_to_compare()[2]),
               pcol = colours_line,
               pfcol = colours_fill,
               plwd = 2)
    
    #Add a legend
    legend(x = 0.5,
           y = 1.35,
           legend = rownames(final_data()[-c(1,2),]),
           bty = "n", pch=20 , col = colours_line, cex = 1.05, pt.cex = 1.5)
  }, height = 375)
  
  # Background colour - changes depending on team selected
  output$players_colour <- renderUI({
    if (input$tabs == "results") {
      if (input$results_team == "Arsenal") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EF0107;}", ".skin-blue .main-header .logo:hover {background-color:", "#EF0107;}", ".skin-blue .main-header .navbar {background-color:", "#EF0107;}")))))
      } 
      else if (input$results_team == "Aston Villa") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#670E36;}", ".skin-blue .main-header .logo:hover {background-color:", "#670E36;}", ".skin-blue .main-header .navbar {background-color:", "#670E36;}")))))
      } 
      else if (input$results_team == "Brighton & Hove Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#0057B8;}", ".skin-blue .main-header .logo:hover {background-color:", "#0057B8;}", ".skin-blue .main-header .navbar {background-color:", "#0057B8;}")))))
      } 
      else if (input$results_team == "Burnley") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6C1D45;}", ".skin-blue .main-header .logo:hover {background-color:", "#6C1D45;}", ".skin-blue .main-header .navbar {background-color:", "#6C1D45;}")))))
      } 
      else if (input$results_team == "Chelsea") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#034694;}", ".skin-blue .main-header .logo:hover {background-color:", "#034694;}", ".skin-blue .main-header .navbar {background-color:", "#034694;}")))))
      } 
      else if (input$results_team == "Crystal Palace") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#1B458F;}", ".skin-blue .main-header .logo:hover {background-color:", "#1B458F;}", ".skin-blue .main-header .navbar {background-color:", "#1B458F;}")))))
      }
      else if (input$results_team == "Everton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003399;}", ".skin-blue .main-header .logo:hover {background-color:", "#003399;}", ".skin-blue .main-header .navbar {background-color:", "#003399;}")))))
      }
      else if (input$results_team == "Fulham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#000000;}", ".skin-blue .main-header .logo:hover {background-color:", "#000000;}", ".skin-blue .main-header .navbar {background-color:", "#000000;}")))))
      }
      else if (input$results_team == "Leeds United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FFCD00;}", ".skin-blue .main-header .logo:hover {background-color:", "#FFCD00;}", ".skin-blue .main-header .navbar {background-color:", "#FFCD00;}")))))
      }
      else if (input$results_team == "Leicester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003090;}", ".skin-blue .main-header .logo:hover {background-color:", "#003090;}", ".skin-blue .main-header .navbar {background-color:", "#003090;}")))))
      }
      else if (input$results_team == "Liverpool") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#C8102E;}", ".skin-blue .main-header .logo:hover {background-color:", "#C8102E;}", ".skin-blue .main-header .navbar {background-color:", "#C8102E;}")))))
      }
      else if (input$results_team == "Manchester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6CABDD;}", ".skin-blue .main-header .logo:hover {background-color:", "#6CABDD;}", ".skin-blue .main-header .navbar {background-color:", "#6CABDD;}")))))
      }
      else if (input$results_team == "Manchester United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#DA291C;}", ".skin-blue .main-header .logo:hover {background-color:", "#DA291C;}", ".skin-blue .main-header .navbar {background-color:", "#DA291C;}")))))
      }
      else if (input$results_team == "Newcastle United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#241F20;}", ".skin-blue .main-header .logo:hover {background-color:", "#241F20;}", ".skin-blue .main-header .navbar {background-color:", "#241F20;}")))))
      }
      else if (input$results_team == "Sheffield United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EE2737;}", ".skin-blue .main-header .logo:hover {background-color:", "#EE2737;}", ".skin-blue .main-header .navbar {background-color:", "#EE2737;}")))))
      }
      else if (input$results_team == "Southampton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#D71920;}", ".skin-blue .main-header .logo:hover {background-color:", "#D71920;}", ".skin-blue .main-header .navbar {background-color:", "#D71920;}")))))
      }
      else if (input$results_team == "Tottenham Hotspur") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#132257;}", ".skin-blue .main-header .logo:hover {background-color:", "#132257;}", ".skin-blue .main-header .navbar {background-color:", "#132257;}")))))
      }
      else if (input$results_team == "West Bromwich Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#122F67;}", ".skin-blue .main-header .logo:hover {background-color:", "#122F67;}", ".skin-blue .main-header .navbar {background-color:", "#122F67;}")))))
      }
      else if (input$results_team == "West Ham United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#7A263A;}", ".skin-blue .main-header .logo:hover {background-color:", "#7A263A;}", ".skin-blue .main-header .navbar {background-color:", "#7A263A;}")))))
      }
      else if (input$results_team == "Wolverhampton Wanderers") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FDB913;}", ".skin-blue .main-header .logo:hover {background-color:", "#FDB913;}", ".skin-blue .main-header .navbar {background-color:", "#FDB913;}")))))
      }
    }
    else if (input$tabs == "player_xG") {
      if (input$xG_team == "All") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#928FA5;}", ".skin-blue .main-header .logo:hover {background-color:", "#928FA5;}", ".skin-blue .main-header .navbar {background-color:", "#928FA5;}")))))
      }
      else if (input$xG_team == "Arsenal") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EF0107;}", ".skin-blue .main-header .logo:hover {background-color:", "#EF0107;}", ".skin-blue .main-header .navbar {background-color:", "#EF0107;}")))))
      } 
      else if (input$xG_team == "Aston Villa") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#670E36;}", ".skin-blue .main-header .logo:hover {background-color:", "#670E36;}", ".skin-blue .main-header .navbar {background-color:", "#670E36;}")))))
      } 
      else if (input$xG_team == "Brighton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#0057B8;}", ".skin-blue .main-header .logo:hover {background-color:", "#0057B8;}", ".skin-blue .main-header .navbar {background-color:", "#0057B8;}")))))
      } 
      else if (input$xG_team == "Burnley") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6C1D45;}", ".skin-blue .main-header .logo:hover {background-color:", "#6C1D45;}", ".skin-blue .main-header .navbar {background-color:", "#6C1D45;}")))))
      } 
      else if (input$xG_team == "Chelsea") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#034694;}", ".skin-blue .main-header .logo:hover {background-color:", "#034694;}", ".skin-blue .main-header .navbar {background-color:", "#034694;}")))))
      } 
      else if (input$xG_team == "Crystal Palace") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#1B458F;}", ".skin-blue .main-header .logo:hover {background-color:", "#1B458F;}", ".skin-blue .main-header .navbar {background-color:", "#1B458F;}")))))
      }
      else if (input$xG_team == "Everton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003399;}", ".skin-blue .main-header .logo:hover {background-color:", "#003399;}", ".skin-blue .main-header .navbar {background-color:", "#003399;}")))))
      }
      else if (input$xG_team == "Fulham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#000000;}", ".skin-blue .main-header .logo:hover {background-color:", "#000000;}", ".skin-blue .main-header .navbar {background-color:", "#000000;}")))))
      }
      else if (input$xG_team == "Leeds") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FFCD00;}", ".skin-blue .main-header .logo:hover {background-color:", "#FFCD00;}", ".skin-blue .main-header .navbar {background-color:", "#FFCD00;}")))))
      }
      else if (input$xG_team == "Leicester") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#003090;}", ".skin-blue .main-header .logo:hover {background-color:", "#003090;}", ".skin-blue .main-header .navbar {background-color:", "#003090;}")))))
      }
      else if (input$xG_team == "Liverpool") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#C8102E;}", ".skin-blue .main-header .logo:hover {background-color:", "#C8102E;}", ".skin-blue .main-header .navbar {background-color:", "#C8102E;}")))))
      }
      else if (input$xG_team == "Manchester City") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#6CABDD;}", ".skin-blue .main-header .logo:hover {background-color:", "#6CABDD;}", ".skin-blue .main-header .navbar {background-color:", "#6CABDD;}")))))
      }
      else if (input$xG_team == "Manchester United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#DA291C;}", ".skin-blue .main-header .logo:hover {background-color:", "#DA291C;}", ".skin-blue .main-header .navbar {background-color:", "#DA291C;}")))))
      }
      else if (input$xG_team == "Newcastle United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#241F20;}", ".skin-blue .main-header .logo:hover {background-color:", "#241F20;}", ".skin-blue .main-header .navbar {background-color:", "#241F20;}")))))
      }
      else if (input$xG_team == "Sheffield United") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#EE2737;}", ".skin-blue .main-header .logo:hover {background-color:", "#EE2737;}", ".skin-blue .main-header .navbar {background-color:", "#EE2737;}")))))
      }
      else if (input$xG_team == "Southampton") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#D71920;}", ".skin-blue .main-header .logo:hover {background-color:", "#D71920;}", ".skin-blue .main-header .navbar {background-color:", "#D71920;}")))))
      }
      else if (input$xG_team == "Tottenham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#132257;}", ".skin-blue .main-header .logo:hover {background-color:", "#132257;}", ".skin-blue .main-header .navbar {background-color:", "#132257;}")))))
      }
      else if (input$xG_team == "West Bromwich Albion") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#122F67;}", ".skin-blue .main-header .logo:hover {background-color:", "#122F67;}", ".skin-blue .main-header .navbar {background-color:", "#122F67;}")))))
      }
      else if (input$xG_team == "West Ham") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#7A263A;}", ".skin-blue .main-header .logo:hover {background-color:", "#7A263A;}", ".skin-blue .main-header .navbar {background-color:", "#7A263A;}")))))
      }
      else if (input$xG_team == "Wolverhampton Wanderers") {
        return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#FDB913;}", ".skin-blue .main-header .logo:hover {background-color:", "#FDB913;}", ".skin-blue .main-header .navbar {background-color:", "#FDB913;}")))))
      }
    }
    else if (input$tabs == "team_xG") {
      return(tags$head(tags$style(HTML(paste('.skin-blue .main-header .logo { background-color:', "#928FA5;}", ".skin-blue .main-header .logo:hover {background-color:", "#928FA5;}", ".skin-blue .main-header .navbar {background-color:", "#928FA5;}")))))
    }
  })
}

shinyApp(ui = ui, server = server)