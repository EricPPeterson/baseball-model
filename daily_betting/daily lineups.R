#daily lineups to create odds
#import libraries needed
library(dplyr)
library(stringr)
library(rvest)


#import final predictions (use final_runs_scored, final_runs_surrendered for daily betting)
final_prediction <- read.csv("~/GitHub/baseball model/Monthly Update/final_prediction_update.csv")
final_prediction <- final_prediction %>%
  select(c('Team', 'final_runs_scored_update', 'final_runs_surr_update'))
colnames(final_prediction) <- c('Team', 'final_runs_scored', 'final_runs_surr')
adjust <- final_prediction %>%
  select(Team, final_runs_scored, final_runs_surr)
rm(final_prediction)

UE_runs <- 1.0865
relief <- 0.46309
WAR <- 9
HFA <- 0.03
full_season <- 162

steamer_pitching <- read.csv("~/GitHub/baseball model/Monthly Update/Pitchers_WAR_Update.csv")
Pitching_Data_2021_starters <- read.csv("~/GitHub/baseball model/Pitching_Data_2021_starters.csv")
Pitching_Data_2021_starters$Team <- sub("^$", "FA", Pitching_Data_2021_starters$Team)
colnames(Pitching_Data_2021_starters)[1] <- 'Name'
steamer_hitting <- read.csv("~/GitHub/baseball model/Monthly Update/Batter_WAR_Update.csv")


#import schedule for the day to input probable lineups and probable starters
home_teams <- data.frame(matrix(ncol = 1, nrow = 0))
away_teams <- data.frame(matrix(ncol = 1, nrow = 0))
url <- 'https://www.cbssports.com/mlb/schedule/'
page <- read_html(url)
today_sched <- page %>% html_nodes('.TeamName a') %>% html_text()

today_sched <- data.frame(today_sched, stringsAsFactors = FALSE)
colnames(today_sched) <- 'CBS_abbrev'

#import cbs abbreviations
cbs_sports_abbrev <- read.csv("~/GitHub/baseball model/schedules/cbs_sports_abbrev.csv")
colnames(cbs_sports_abbrev)[1] <- 'CBS_abbrev'

#join to change to correct abbreviations
today_sched <- left_join(today_sched, cbs_sports_abbrev, by = 'CBS_abbrev') %>%
  dplyr :: select(Team)

row_odd <- seq_len(nrow(today_sched)) %% 2
away_teams <- data.frame(today_sched[row_odd == 1,])
colnames(away_teams) <- 'away_teams'
home_teams <- data.frame(today_sched[row_odd == 0,])
colnames(home_teams) <- 'home_teams'

sched <- bind_cols(away_teams, home_teams)
sched$away_team_win_pct <- NA
sched$home_team_win_pct <- NA
sched$away_team_game_win <- NA
sched$home_team_game_win <- NA
sched$away_team_odds <- NA
sched$home_team_odds <- NA

#log5 calculation
#this calculates probabilities of team A beating team B
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}

#American Odds Calc
American_Odds <- function(X){
  if(X > 2){
    output = (X - 1) * 100
    } else {
    output = -100 / (X- 1) 
    }
  return(output)
  }


##########################################################################################################3
##here we'll download the final lineups once they're set
#need to import final lineups to set daily lineup
url_lineups <- 'https://www.mlb.com/starting-lineups/2022-05-01'
page_lineups <- read_html(url_lineups)
players <- page_lineups %>% html_nodes('.starting-lineups__player--link') %>% html_text()
pitchers <- page_lineups %>% html_nodes('.starting-lineups__pitcher-name .starting-lineups__pitcher--link') %>% html_text()
starting_lineups <- data.frame(players, stringsAsFactors = FALSE)
colnames(starting_lineups) <- c('Name')

starting_pitchers <- data.frame(pitchers, stringsAsFactors = FALSE)
colnames(starting_pitchers) <- c('Name')
  
starting_pitchers <- left_join(starting_pitchers, steamer_pitching, by = 'Name') %>%
  dplyr :: select(c(Name, Team, ERA, adj_WAR, GS, IP))

starting_lineups <- left_join(starting_lineups, steamer_hitting, by = 'Name') %>%
  dplyr :: select(c(Name, Team, adj_WAR))

starting_lineups <- starting_lineups %>%
  dplyr :: filter(is.na(Team) == F)

starting_lineups <- starting_lineups %>%
  dplyr :: filter(WAR != 'NA')

daily_WAR <- starting_lineups %>%
  group_by(Team) %>%
  dplyr :: summarise(sumWAR_daily = sum(adj_WAR))

yearly_WAR <- steamer_hitting %>%
  group_by(Team) %>%
  dplyr :: summarise(sumWAR_yearly = sum(WAR))

daily_adjustment <- left_join(daily_WAR, yearly_WAR, by = 'Team') %>%
  select(Team, sumWAR_daily, sumWAR_yearly) %>%
  mutate(WAR_diff = sumWAR_daily - sumWAR_yearly) %>%
  dplyr :: filter(Team != "")

#mutate data to calculate how many runs a team would give up pitched by the probables
pitchers_WAR <- starting_pitchers %>%
  dplyr :: mutate(annual_WAR = (full_season / GS) * adj_WAR) %>%
  dplyr :: select(c(Name, Team, adj_WAR, annual_WAR))

pitching_sumWAR <- Pitching_Data_2021_starters %>%
  dplyr :: group_by(Team) %>%
  dplyr :: summarise(sumWAR = sum(WAR))

pitchers_WAR <- left_join(pitchers_WAR, pitching_sumWAR, by = 'Team') %>%
  dplyr :: select(c(Name, Team, annual_WAR, sumWAR)) %>%
  dplyr :: mutate(WAR_diff = annual_WAR - sumWAR)

pitchers_WAR <- left_join(pitchers_WAR, adjust, by = 'Team') %>%
  dplyr :: select(c(Team, WAR_diff, final_runs_surr)) %>%
  dplyr :: mutate(adj_R_allowed = final_runs_surr + WAR_diff)


#######################final adjustment for set lineups######################################################
daily_adjustment <- left_join(daily_adjustment, adjust, by = 'Team') %>%
  dplyr :: select(Team, sumWAR_daily, sumWAR_yearly, WAR_diff, final_runs_scored) %>%
  dplyr :: mutate(adj_runs_scored = final_runs_scored + (WAR_diff * WAR))

daily_adjustment <- left_join(daily_adjustment, pitchers_WAR, by = 'Team') %>%
  select(Team, adj_runs_scored, adj_R_allowed) %>%
  mutate(daily_run_diff = adj_runs_scored - adj_R_allowed)

#win pct = 0.5000 + 0.0006281 * run_diff
five_hundred <- .5000
coef_win_pct <- 0.0006281
daily_adjustment <- daily_adjustment %>%
  mutate(win_pct = five_hundred + (coef_win_pct * daily_run_diff))

daily_adjustment <- daily_adjustment %>% 
  filter_all(~ !is.na(.))


for(i in 1:nrow(sched)){
  home_team <- sched[i,2]
  away_team <- sched[i,1]
  
  if(home_team %in% daily_adjustment$Team == F || away_team %in% daily_adjustment$Team == F) next
  
  home_team_win_pct <- daily_adjustment %>%
    dplyr :: filter(Team == home_team) %>%
    dplyr :: select(win_pct)
  
  away_team_win_pct <- daily_adjustment %>%
    dplyr :: filter(Team == away_team) %>%
    dplyr :: select(win_pct)
  
  sched[i,4] <- home_team_win_pct
  sched[i,3] <- away_team_win_pct
  
  sched[i,6] <- log5(home_team_win_pct,away_team_win_pct) + HFA
  sched[i,5] <- 1 - sched[i,6]
  
  sched[i,8] <- 1/sched[i,6]
  sched[i,7] <- 1/sched[i,5]
}


setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/schedules")
write.csv(sched, 'today_bets.csv', row.names = FALSE)

starting_lineups %>% dplyr :: count(Team)

