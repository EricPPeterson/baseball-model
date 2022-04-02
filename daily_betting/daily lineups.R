#daily lineups to create odds
#import libraries needed
library(dplyr)
library(stringr)
library(rvest)

#import final predictions (use final_runs_scored, final_runs_surrendered for daily betting)
final_prediction<- read.csv("~/GitHub/baseball model/daily_betting/final_prediction")
adjust <- final_prediction %>%
  select(Team, final_runs_scored, final_runs_surr)
rm(final_prediction)
UE_runs <- 1.0865
relief <- 0.46309
WAR <- 9
HFA <- 0.04
full_season <- 162
steamer_pitching <- read.csv("~/GitHub/baseball model/steamerpitching2022_final.csv", header=TRUE)
colnames(steamer_pitching)[1] <- 'Name'
Pitching_Data_2021_starters <- read.csv("~/GitHub/baseball model/Pitching_Data_2021_starters.csv")
Pitching_Data_2021_starters$Team <- sub("^$", "FA", Pitching_Data_2021_starters$Team)
colnames(Pitching_Data_2021_starters)[1] <- 'Name'
steamer_hitting <- read.csv("~/GitHub/baseball model/steamerhitting2022_final.csv")
colnames(steamer_hitting)[1] <- 'Name'

#import schedule for the day to input probable lineups and probable starters
sched <- read.csv("~/GitHub/baseball model/schedules/MLB schedule.csv")
sched_probables <- read.csv("~/GitHub/baseball model/schedules/MLB schedule.csv")

#log5 calculation
#this calculates probabilities of team A beating team B
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}

#set lineup with probable pitchers and standard lineup
url_probables <- 'https://www.mlb.com/probable-pitchers/2022-04-07'
#read url into correct format
page_probables <- read_html(url_probables)
#turn page_probables into dataframe
probables <- page_probables %>% html_nodes('.probable-pitchers__pitcher-name-link') %>% html_text()
probable_pitchers <- data.frame(probables, stringsAsFactors = FALSE)
colnames(probable_pitchers) <- c('Name')
probable_pitchers <- data.frame(probable_pitchers, stringsAsFactors = FALSE)

#join probables dataframe with player data 
probable_pitchers <- probable_pitchers %>% left_join(steamer_pitching, by = 'Name') %>%
  dplyr :: select(c(Name, Team, ERA, WAR, GS, IP))

#mutate data to calculate how many runs a team would give up pitched by the probables
probables_WAR <- probable_pitchers %>%
  dplyr :: mutate(annual_WAR = (full_season / GS) * WAR,) %>%
  dplyr :: select(c(Name, Team, WAR, annual_WAR))

pitching_sumWAR <- Pitching_Data_2021_starters %>%
  dplyr :: group_by(Team) %>%
  dplyr :: summarise(sumWAR = sum(WAR))

probables_WAR <- left_join(probables_WAR, pitching_sumWAR, by = 'Team') %>%
  dplyr :: select(c(Name, Team, annual_WAR, sumWAR)) %>%
  dplyr :: mutate(WAR_diff = annual_WAR - sumWAR) %>%
  dplyr :: mutate(run_diff = -WAR_diff * WAR)

#filter down just to standard starters
starting_players <- steamer_hitting %>%
  dplyr :: filter(Starters == 'X') %>%
  dplyr :: select(c(Name, Team, adj_WAR))

#calculate starters WAR vs. total WAR from offensive players
starters_WAR <- starting_players %>%
  dplyr :: group_by(Team) %>%
  dplyr :: summarise(sumWAR_daily = sum(adj_WAR))

#total yearly war from offensive players
yearly_WAR <- steamer_hitting %>%
  dplyr :: group_by(Team) %>%
  dplyr :: summarise(sumWAR_yearly = sum(WAR))

#calculate total runs given up by a team that only starts the probable starter
probables_WAR <- left_join(probables_WAR, adjust, by = 'Team') %>%
  dplyr :: select(c(Team, final_runs_surr, run_diff)) %>%
  dplyr :: mutate(adj_runs_surr = final_runs_surr + run_diff)

#calculate total runs scored with standard starters
starters_WAR <- left_join(starters_WAR, yearly_WAR, by = 'Team') %>%
  dplyr :: select(c(Team, sumWAR_yearly, sumWAR_daily)) %>%
  dplyr :: mutate(run_diff = (sumWAR_daily - sumWAR_yearly) * WAR)

starters_WAR <- left_join(starters_WAR, adjust, by = 'Team') %>%
  dplyr :: select(c(Team, final_runs_scored, run_diff)) %>%
  dplyr :: mutate(off_runs_diff = final_runs_scored + run_diff)

probables_adjustment <- left_join(probables_WAR, starters_WAR, by = 'Team') %>%
  dplyr :: select(c(Team, off_runs_diff, adj_runs_surr)) %>%
  dplyr :: mutate(adj_run_diff = off_runs_diff - adj_runs_surr)

#win pct = 0.5000 + 0.0006281 * run_diff
five_hundred <- .5000
coef_win_pct <- 0.0006281
probables_adjustment <- probables_adjustment %>%
  mutate(win_pct = five_hundred + (coef_win_pct * adj_run_diff))

#calcuate out daily odds of teams winning to place bets
for(i in 1:nrow(sched_probables)){
  home_team <- sched_probables[i,1]
  away_team <- sched_probables[i,5]
  
  home_team_win_pct <- probables_adjustment %>%
    dplyr :: filter(Team == home_team) %>%
    dplyr :: select(win_pct) %>%
    mutate(win_pct = win_pct + HFA)
  away_team_win_pct <- probables_adjustment %>%
    dplyr :: filter(Team == away_team) %>%
    dplyr :: select(win_pct) %>%
    mutate(win_pct = win_pct - HFA)
  
  sched_probables[i,2] <- home_team_win_pct
  sched_probables[i,6] <- away_team_win_pct
  
  sched_probables[i,3] <- log5(home_team_win_pct,away_team_win_pct)
  sched_probables[i,7] <- 1 - sched_probables[i,3]
  
  sched_probables[i,4] <- 1/sched_probables[i,3]
  sched_probables[i,8] <- 1/sched_probables[i,7]
}

setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/schedules")
write.csv(sched_probables, 'probables_bets.csv', row.names = FALSE)

##########################################################################################################3
##here we'll download the final lineups once they're set
#need to import final lineups to set daily lineup
url_lineups <- 'https://www.mlb.com/starting-lineups/2022-03-31'
page_lineups <- read_html(url_lineups)
players <- page_lineups %>% html_nodes('.starting-lineups__player--link') %>% html_text()
pitchers <- page_lineups %>% html_nodes('.starting-lineups__pitcher-name .starting-lineups__pitcher--link') %>% html_text()
starting_lineups <- data.frame(players, stringsAsFactors = FALSE)
colnames(starting_lineups) <- c('Name')
starting_pitchers <- data.frame(pitchers, stringsAsFactors = FALSE)
colnames(starting_pitchers) <- c('Name')
  

starting_pitchers <- left_join(starting_pitchers, steamer_pitching, by = 'Name') %>%
  select(c(Name, Team, ERA, WAR, GS, IP))

starting_lineups <- left_join(starting_lineups, steamer_hitting, by = 'Name') %>%
  dplyr :: select(c(Name, Team, adj_WAR))

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
  dplyr :: mutate(annual_WAR = (full_season / GS) * WAR) %>%
  dplyr :: select(c(Name, Team, WAR, annual_WAR))

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


for(i in 1:nrow(sched)){
  home_team <- sched[i,1]
  away_team <- sched[i,5]
  
  home_team_win_pct <- daily_adjustment %>%
    dplyr :: filter(Team == home_team) %>%
    dplyr :: select(win_pct) %>%
    mutate(win_pct = win_pct + HFA)
  away_team_win_pct <- daily_adjustment %>%
    dplyr :: filter(Team == away_team) %>%
    dplyr :: select(win_pct) %>%
    mutate(win_pct = win_pct - HFA)
  
  sched[i,2] <- home_team_win_pct
  sched[i,6] <- away_team_win_pct
  
  sched[i,3] <- log5(home_team_win_pct,away_team_win_pct)
  sched[i,7] <- 1 - sched[i,3]
  
  sched[i,4] <- 1/sched[i,3]
  sched[i,8] <- 1/sched[i,7]
}

setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/schedules")
write.csv(sched, 'today_bets.csv', row.names = FALSE)


