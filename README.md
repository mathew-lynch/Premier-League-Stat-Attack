# Premier League Stat Attack
An interactive RShiny dashboard showcasing Premier League results and xG data sourced from ESPN and Understat, respectively.

The inspiration for this analysis came from George Pipis' R-bloggers post on 7th March 2021 about scraping NBA basketball data from ESPN (https://www.r-bloggers.com/2021/03/how-to-build-a-predictive-model-for-nba-games/). Some of the code used to scrape data from Understat was sourced from ewenme's understatr package on GitHub (https://github.com/ewenme/understatr).

The dashboard is hosted by shinyapps.io and is available at https://mlynch98.shinyapps.io/Premier_League_Stat_Attack/

## The dashboard

This RShiny dashboard showcases Premier League results and xG data from the 2020/21 season. Expected goals has become an important metric in analysing over- and under-performing teams and players. In spite of its limitations, it gives a fascinating insight into performance and incorporates analytics and predictive modelling into understanding the complexities of the beautiful game.  

### Results

•	Data sourced from ESPN.co.uk Premier League results (espn.co.uk/football/)

• The user selects a team from a drop-down menu and is presented with an overview of their performance in the 2020/21 season, such as games played, won, drawn and lost as well a full breakdown of their results

• The team's performance relative to their league rivals is presented in terms of their league rank for wins, draws and losses both home and away

• Interactive pie charts allow the user to filter a team's results by home/away and by win/draw/loss

### Team xG (expected goals)

• Data sourced from understat.com Expected Goals (xG) (https://understat.com/league/EPL)

• A series of visualisations are presented, including the Premier League table if it was decided by expected points (xPTS), a comparison between goals and xG to identify underperforming and overperforming teams, and a graphic identifying the quality of defences and attacks based on a comparison between xG and xGA (expected goals against)

• In addition to these visualisations, the user can compare the expected points, goals and goals against of two teams side-by-side, allowing for more in-depth comparisons to be made

### Player xG

• Data sourced from understat.com Expected Goals (xG) (https://understat.com/league/EPL)

• The user has the option to compare players across all 20 Premier League clubs, or filter by a particular team

• A radar chart can be used to compare two players across a series of metrics, and both tabular and graphical representations of a player's xG and xA delta (difference between expected and observed output) are also included

## The code

**scrape_results**:
Scrapes results for every Premier League team from https://www.espn.co.uk and returns a tibble of results

**process_data**:
Takes the scraped results and calculates home/away win/draw/loss totals and percentages for a SPECIFIC team. The function requires a tibble containing the results, and the team name

**home_and_away**:
Calculates proportions of wins/draws/losses

**summary_all**:
Calculates win/draw/loss totals and percentages for home and away games for EVERY team - this is required when calculating team rankings

**home_and_away_rankings**:
Calculates home and away rankings for wins, draws and losses

**teams_data**:
Scrapes Premier League team xG data from https://understat.com/league/EPL and returns a tibble of results

**players_data**:
Scrapes Premier League player xG data from https://understat.com/league/EPL and returns a tibble of results
