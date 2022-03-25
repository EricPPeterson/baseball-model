library(dplyr)
library(stringr)
library(rvest)

#import final predictions (use final_runs_scored, final_runs_surrendered for daily betting)
final_prediction<- read.csv("~/GitHub/baseball model/daily_betting/final_prediction")
adjust <- final_prediction %>%
  select(Team, final_runs_scored, final_runs_surr)
UE_runs <- 1.0865
relief <- 0.46309
WAR <- 9
HFA <- 0.04

#need to import final lineups to set daily lineup
url <- 'https://www.mlb.com/starting-lineups/2022-03-24'
page <- read_html(url)
players <- page %>% html_nodes('.starting-lineups__player--link') %>% html_text()
pitchers <- page %>% html_nodes('.starting-lineups__pitcher-name .starting-lineups__pitcher--link') %>% html_text()
starting_lineups <- data.frame(players, stringsAsFactors = FALSE)
colnames(starting_lineups) <- c('Name')
starting_pitchers <- data.frame(pitchers, stringsAsFactors = FALSE)
colnames(starting_pitchers) <- c('Name')
  
steamer_pitching <- read.csv("~/GitHub/baseball model/steamerpitching2022_final.csv", header=TRUE)
colnames(steamer_pitching)[1] <- 'Name'
steamer_hitting <- read.csv("~/GitHub/baseball model/steamerhitting2022_final.csv")
colnames(steamer_hitting)[1] <- 'Name'

starting_pitchers <- left_join(starting_pitchers, steamer_pitching, by = 'Name') %>%
  select(c(Name, Team, ERA, WAR, GS, IP))

starting_lineups <- left_join(starting_lineups, steamer_hitting, by = 'Name') %>%
  dplyr :: select(c(Name, Team, WAR))
colnames(starting_lineups)[2] <- 'Team' 

starting_lineups <- starting_lineups %>%
  dplyr :: filter(WAR != 'NA')

daily_WAR <- starting_lineups %>%
  group_by(Team) %>%
  dplyr :: summarise(sumWAR_daily = sum(WAR))

yearly_WAR <- steamer_hitting %>%
  group_by(Team) %>%
  dplyr :: summarise(sumWAR_yearly = sum(WAR))

daily_adjustment <- left_join(daily_WAR, yearly_WAR, by = 'Team') %>%
  select(Team, sumWAR_daily, sumWAR_yearly) %>%
  mutate(WAR_diff = sumWAR_daily - sumWAR_yearly) %>%
  dplyr :: filter(Team != "")

starting_pitchers <- starting_pitchers %>%
  mutate(total_ER = ERA * 162,
         avg_IP = IP / GS,
         avg_ER = total_ER * (avg_IP / 9),
         relief_ER = (9 - avg_IP) * 162 * relief,
         adj_R_allowed = (avg_ER + relief_ER) * UE_runs)


daily_adjustment <- left_join(daily_adjustment, final_prediction, by = 'Team') %>%
  select(Team, sumWAR_daily, sumWAR_yearly, WAR_diff, final_runs_scored) %>%
  mutate(adj_runs_scored = final_runs_scored + (WAR_diff * WAR))

daily_adjustment <- left_join(daily_adjustment, starting_pitchers, by = 'Team') %>%
  select(Team, sumWAR_daily, sumWAR_yearly, WAR_diff, adj_runs_scored, adj_R_allowed) %>%
  mutate(daily_run_diff = adj_runs_scored - adj_R_allowed)

#win pct = 0.5000 + 0.0006281 * run_diff
five_hundred <- .5000
coef_win_pct <- 0.0006281
daily_adjustment <- daily_adjustment %>%
  mutate(win_pct = five_hundred + (coef_win_pct * daily_run_diff))


#import schedule
sched <- read.csv("~/GitHub/baseball model/schedules/MLB schedule.csv")

#log5 calculation
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}

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
write.csv(sched, 'today_bets', row.names = FALSE)


