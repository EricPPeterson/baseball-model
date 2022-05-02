#monthly model update
library(tidyverse)
library(readr)
library(plyr)
library(dplyr)
library(lookup)
library(baseballr)


#download data for update
#download final prediction dataset from preseason prediction
final_prediction <- read.csv("~/GitHub/baseball model/daily_betting/final_prediction")

#download April pitching and hitting data to blend with the preseason prediction
Batting_April <- read.csv("~/GitHub/baseball model/Monthly Update/Batting_April.csv")
colnames(Batting_April)[1] <- 'Team'
colnames(Batting_April)[8] <-'BB_rate'
colnames(Batting_April)[9] <- 'K_rate'

Pitching_April <- read.csv("~/GitHub/baseball model/Monthly Update/Pitching_April.csv")
colnames(Pitching_April)[1] <- 'Team'
colnames(Pitching_April)[8] <-'K_per_9'
colnames(Pitching_April)[9] <- 'BB_per_9'
colnames(Pitching_April)[10] <- 'HR_per_9'

#download individual players to update WAR
IndividualBatters_April <- read.csv("~/GitHub/baseball model/Monthly Update/IndividualBatters_April.csv")
IndividualPitchers_April <- read.csv("~/GitHub/baseball model/Monthly Update/IndividualPitchers_April.csv")
colnames(IndividualBatters_April)[1] <- 'Name'
colnames(IndividualPitchers_April)[1] <- 'Name'
steamerpitching2022_final <- read.csv("~/GitHub/baseball model/steamerpitching2022_final.csv")
colnames(steamerpitching2022_final)[1] <- 'Name'
steamerpitching2022_final <- steamerpitching2022_final %>%
  filter(Team != "")
steamerhitting2022_final <- read.csv("~/GitHub/baseball model/steamerhitting2022_final.csv")
colnames(steamerhitting2022_final)[1] <- 'Name'
steamerhitting2022_final <- steamerhitting2022_final %>%
  filter(Team != "")

#Download 2022 Standings
AL_East_Std <- standings_on_date_bref(date = '2022-04-28', division = 'AL East')
AL_East_Std$Division <- 'AL East'
AL_Cent_Std <- standings_on_date_bref(date = '2022-04-28', division = 'AL Central')
AL_Cent_Std$Division <- 'AL Cent'
AL_West_Std <- standings_on_date_bref(date = '2022-04-28', division = 'AL West')
AL_West_Std$Division <- 'AL West' 
NL_East_Std <- standings_on_date_bref(date = '2022-04-28', division = 'NL East')
NL_East_Std$Division <- 'NL East'
NL_Cent_Std <- standings_on_date_bref(date = '2022-04-28', division = 'NL Central')
NL_Cent_Std$Division <- 'NL Cent'
NL_West_Std <- standings_on_date_bref(date = '2022-04-28', division = 'NL West')
NL_West_Std$Division <- 'NL West'

#combine standings to one dataframe
Std_2022 <- bind_rows(AL_East_Std, AL_Cent_Std, AL_West_Std, NL_East_Std, NL_Cent_Std, NL_West_Std)
colnames(Std_2022)[1] <- 'Team'
Std_2022 <- Std_2022 %>%
  dplyr :: mutate(Games = W + L, 
         percent_season = Games / 162)


#combine batting data with games played
Batting_April <- left_join(Batting_April, Std_2022, by = 'Team') %>%
  select(c(Team, Games, percent_season, H, R, BB_rate, ISO, BABIP, WAR))
Batting_April[]<-lapply(Batting_April,gsub,pattern="%",fixed = TRUE,replacement="")
Batting_April[, c(2:8)] <- sapply(Batting_April[, c(2:8)], as.numeric) 

#add percent_season to players
IndividualBatters_April <- left_join(IndividualBatters_April, Std_2022, by = 'Team') %>%
  dplyr :: select(c(Name, Team, PA, WAR, percent_season))
IndividualPitchers_April <- left_join(IndividualPitchers_April, Std_2022, by = 'Team') %>%
  dplyr :: select(c(Name, Team, WAR, percent_season))
colnames(IndividualPitchers_April)[3] <- 'update_WAR'

#inner-join preseason prediction with update of April Pitchers
Pitchers_WAR_Update <- inner_join(steamerpitching2022_final, IndividualPitchers_April, by = c('Name', 'Team')) %>%
  dplyr :: select(c(Name, Team, GS, ERA, IP, WAR, update_WAR, percent_season))

#blend preseason WAR with April WAR
Pitchers_WAR_Update <- Pitchers_WAR_Update %>%
  dplyr :: mutate(adj_WAR = update_WAR * percent_season + WAR * (1-percent_season))

#batters blend preseason WAR with April WAR
PA_year <- 600
IndividualBatters_April$WAR_conversion <- IndividualBatters_April$PA / (PA_year * IndividualBatters_April$percent_season)
IndividualBatters_April$update_WAR <- ifelse(IndividualBatters_April$WAR_conversion > 1,IndividualBatters_April$WAR,IndividualBatters_April$WAR / IndividualBatters_April$WAR_conversion)

#inner-join preseason prediction with update of April Batters
Batters_WAR_Update <- inner_join(steamerhitting2022_final, IndividualBatters_April, by = c('Name', 'Team')) %>%
  dplyr :: select(c(Name, Team, adj_WAR, update_WAR, percent_season))

Batters_WAR_Update <- Batters_WAR_Update %>%
  dplyr :: mutate(adj_WAR = adj_WAR * (1-percent_season) + update_WAR * percent_season)

#variable creation
Batting_April <- Batting_April %>%
  dplyr :: mutate(hit_per_run = H/R,
         BB_rate = BB_rate/100)

#offense 
#hit_per_run = 2.5708 - 6.6440 * BB_rate - 3.2686 * ISO + 1.2421 * BABIP
Batting_April <- Batting_April %>%
  dplyr :: mutate(hit_per_run = 2.5708 - 6.6440 * BB_rate - 3.2686 * ISO + 1.2421 * BABIP,
         exp_runs_scored_update = H / hit_per_run)
final_prediction_update <- left_join(final_prediction, Batting_April, by = 'Team') %>%
  dplyr :: select(c(Team, final_runs_scored, final_runs_surr, percent_season, exp_runs_scored_update))

#blend final runs scored with new season data
final_prediction_update <- final_prediction_update %>%
  dplyr :: mutate(final_runs_scored_update = exp_runs_scored_update + ((1-percent_season) * final_runs_scored))

#pitching
#ERA = -1.05469 - 0.23055 * K_per_9 + 0.40402 * BB_per_9 + 1.56898 * HR_per_9 + 13.91693 * BABIP
Pitching_April <- Pitching_April %>%
  dplyr :: mutate(update_ER_allowed = (-1.05469 -0.23055 * K_per_9 + 0.40402 * BB_per_9 + 1.56898 * HR_per_9 + 13.91693 * BABIP) * (IP/9))

#add unearned runs
#total_runs / earned_runs = 1.086492
UE_runs <- 1.086492
relief <- 0.46309
Pitching_April <- Pitching_April %>%
  dplyr :: mutate(update_runs_surr = update_ER_allowed * UE_runs)

#combine with final prediction / pitching
final_prediction_update <- left_join(final_prediction_update, Pitching_April, by = 'Team') %>%
  dplyr :: select(c(Team, final_runs_scored, final_runs_surr, percent_season, exp_runs_scored_update, final_runs_scored_update, update_runs_surr))

#update runs surrendered
final_prediction_update <- final_prediction_update %>%
  dplyr :: mutate(final_runs_surr_update = update_runs_surr + (final_runs_surr * (1-percent_season)))

#win pct update
#win pct = 0.5000 + 0.0006281 * run_diff
five_hundred <- .5000
coef_win_pct <- 0.0006281
final_prediction_update <- final_prediction_update %>%
  dplyr :: mutate(win_pct_update = five_hundred + (coef_win_pct * (final_runs_scored_update - final_runs_surr_update)),
                  total_wins_update = 162 * win_pct_update)


setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/Monthly Update")
write.csv(final_prediction_update, 'final_prediction_update.csv', row.names = FALSE)
write.csv(Batters_WAR_Update, 'Batter_WAR_Update.csv', row.names = FALSE)
write.csv(Pitchers_WAR_Update, 'Pitchers_WAR_Update.csv', row.names = FALSE)
