#final model build
library(tidyverse)
library(readr)
library(rio)
library(olsrr)
library(baseballr)
library(reshape2)
library(Lahman)
library(rvest)
library(xml2)
library(plyr)
library(dplyr)


#Download 2021 Standings
AL_East_Std <- standings_on_date_bref(date = '2021-10-03', division = 'AL East')
AL_East_Std$Division <- 'AL East'
AL_Cent_Std <- standings_on_date_bref(date = '2021-10-03', division = 'AL Central')
AL_Cent_Std$Division <- 'AL Cent'
AL_West_Std <- standings_on_date_bref(date = '2021-10-03', division = 'AL West')
AL_West_Std$Division <- 'AL West' 
NL_East_Std <- standings_on_date_bref(date = '2021-10-03', division = 'NL East')
NL_East_Std$Division <- 'NL East'
NL_Cent_Std <- standings_on_date_bref(date = '2021-10-03', division = 'NL Central')
NL_Cent_Std$Division <- 'NL Cent'
NL_West_Std <- standings_on_date_bref(date = '2021-10-03', division = 'NL West')
NL_West_Std$Division <- 'NL West'

#combine standings to one dataframe
Std_2021 <- bind_rows(AL_East_Std, AL_Cent_Std, AL_West_Std, NL_East_Std, NL_Cent_Std, NL_West_Std)
colnames(Std_2021)[1] <- 'Team'

#teams data from Lahman database
#get team data back to 2000 to look at run differential linear model
#pull in the following data for each team:
# Team Name, Year, AL/NL, Games, Wins, Losses, Runs, Runs Allowed
teams <- Teams %>%  
  dplyr :: filter(yearID > 2000) %>%  
  select(teamID, yearID, lgID, G, W, L, R, RA)

#team stats data for analysis
#get team data back to 2018 to create linear model for hits/run
#pull in the following data for each team:
#Team, Year, AL/NL, Games, Wins, Losses, Runs, Runs Allowed, At Bats, Hits, Doubles, Triples, Home Runs, Walks, 
#Strike Outs, Earned Runs, Earned Run Average, Hits Allowed, HR Allowed, Walks Allowed, Strike Outs Allowed, Hit By Pitch,
#Sacrifice Flies
team_stats <- Teams %>% 
  dplyr :: filter(yearID >= 2018) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA, AB, H, X2B, X3B, HR, BB, SO, ER, ERA, HA, HRA, BBA, SOA, HBP, SF)

#earned run adjustment
#get earned run data back to 2000 to see what % of runs are earned run on average
#pull following data:
#Team, Year, Runs Allowed, Earned Runs, Errors 
earned_run_data <- Teams %>%
  dplyr :: filter(yearID > 2000) %>%
  select(teamID, yearID, RA, ER, E)

#create a variable to see what % of runs to add to earned runs allowed to get total runs allowed.
by_year <- earned_run_data %>%
  dplyr :: group_by(yearID) %>%
  dplyr :: summarize(yearly_avg = mean(RA/ER))

#create a plot to see what the graph of unearned runs percentage looks like.
UE_run_plot <- ggplot(by_year, aes(x = yearID, y = yearly_avg)) + geom_line() + 
  scale_x_continuous('Year') + 
  scale_y_continuous('Unearned Runs Percentage')
UE_run_plot

#2021 data for building model
#use 2021 data to see how many runs teams 'should' have scored and 'should' have allowed after removing cluster luck.
TeamData2021 <- read.csv("~/GitHub/baseball model/2021TeamData.csv")

#create teams df for creating lm for run differential
teams <- teams %>% 
  mutate (RD = R - RA, Wpct = W / (W + L))

#create a plot to see what the graph of run differential versus win percentage is.
run_diff <- ggplot(teams, aes(x = RD, y = Wpct)) + geom_point() + 
  scale_x_continuous('Run Differential') + 
  scale_y_continuous('Win Pct')

run_diff + geom_smooth(method = 'lm', se = F)


#check regression of win pct vs. run diff
#see if the variables are significant
lm_RD <- lm(Wpct ~ RD, data = teams)

summary(lm_RD)

#save coefficients of linear model to predict wins for teams based on predicted runs scored and runs allowed.
#coef1 = slope.
coef_RD <- lm_RD$coefficients

#check residuals
#see if linear model residuals are random and normally distributed
library(lmtest)
library(fBasics)
dwtest(lm_RD)
jarqueberaTest(lm_RD$resid) #Test residuals for normality
resid_RD <- lm_RD$residuals

plot(lm_RD, which = 1, col = c('blue'))
plot(lm_RD, which = 2, col = c('red'))

#residuals look normal and randomly distributed

#use below formula to turn run differential into predicted win %
#win pct = 0.5000 + 0.0006281 * run_diff


#expected Wpct
teams <- teams %>% 
  mutate(ExpWpct = (R-RA) * coef_RD[2] + coef_RD[1])

teams <- teams %>%
  mutate(resExpWpct = Wpct - ExpWpct)

teams %>%
  summarize(rmse = sqrt(mean(resExpWpct^2)))

#incremental runs needed for a win
#create table to confirm how many runs added to run differential equals 1 win.
IR <- function(RS = 5, RA = 5){
  (RS^2 + RA^2)^2 / (2 * RS * RA^2)
}

ir_table <- expand_grid(RS = seq(3,6,0.25), RA = seq(3,6,.25))

ir_table %>%
  mutate(IRW = IR(RS,RA)) %>%
  spread(key = RA, value = IRW, sep = '=') %>%
  round(2)

#runs scored MLB 2021 = 9 per. 1 WAR = 9 runs (put calc here)

#hitting data
#team data
Batting_Data_2021 <- read.csv("~/GitHub/baseball model/Batting_Data_2021.csv")
Batting_Data_2021$Season <- 2021
Batting_Data_2020 <- read.csv("~/GitHub/baseball model/Batting_Data_2020.csv")
Batting_Data_2020$Season <- 2020
Batting_Data_2019 <- read.csv("~/GitHub/baseball model/Batting_Data_2019.csv")
Batting_Data_2019$Season <- 2019
Batting_Data <- rbind(Batting_Data_2019, Batting_Data_2020, Batting_Data_2021)
colnames(Batting_Data)[1] <- colnames(Batting_Data_2021)[1] <- 'Team'
colnames(Batting_Data)[8] <- colnames(Batting_Data_2021)[8] <-'BB_rate'
colnames(Batting_Data)[9] <- colnames(Batting_Data_2021)[9] <- 'K_rate'
Batting_Data[]<-lapply(Batting_Data,gsub,pattern="%",fixed=TRUE,replacement="")
Batting_Data_2021[]<-lapply(Batting_Data_2021,gsub,pattern="%",fixed=TRUE,replacement="")
Batting_Data[, c(2:341)] <- sapply(Batting_Data[, c(2:341)], as.numeric)
Batting_Data_2021[, c(2:341)] <- sapply(Batting_Data_2021[, c(2:341)], as.numeric) 

#variable creation
Batting_Data <- Batting_Data %>%
  mutate(hit_per_run = H/R,
         HR_rate = HR/AB,
         BB_rate = BB_rate/100,
         K_rate = K_rate/100)

Batting_Data_2021 <- Batting_Data_2021 %>%
  mutate(hit_per_run = H/R,
         HR_rate = HR/AB,
         BB_rate = BB_rate/100,
         K_rate = K_rate/100)

#steamer projection
steamer_hitting <- read.csv("~/GitHub/baseball model/steamerhitting2022_final.csv")
colnames(steamer_hitting)[1] <- 'Name'
steamer_hitting$Team <- sub("^$", "FA", steamer_hitting$Team)

#pitching data
#team data
Pitching_Data_2021 <- read.csv("~/GitHub/baseball model/Pitching_Data_2021.csv")
Pitching_Data_2021$Season <- 2021
Pitching_Data_2020 <- read.csv("~/GitHub/baseball model/Pitching_Data_2020.csv")
Pitching_Data_2020$Season <- 2020
Pitching_Data_2019 <- read.csv("~/GitHub/baseball model/Pitching_Data_2019.csv")
Pitching_Data_2019$Season <- 2019
Pitching_Data <- rbind(Pitching_Data_2019, Pitching_Data_2020, Pitching_Data_2021)
colnames(Pitching_Data)[1] <- colnames(Pitching_Data_2021)[1] <- 'Team'
colnames(Pitching_Data)[8] <- colnames(Pitching_Data_2021)[8] <-'K_per_9'
colnames(Pitching_Data)[9] <- colnames(Pitching_Data_2021)[9] <- 'BB_per_9'
colnames(Pitching_Data)[10] <- colnames(Pitching_Data_2021)[10] <- 'HR_per_9'
colnames(Pitching_Data)[12] <- colnames(Pitching_Data_2021)[12] <- 'LOB_pct'
colnames(Pitching_Data)[13] <- colnames(Pitching_Data_2021)[13] <- 'GB_pct'
colnames(Pitching_Data)[14] <- colnames(Pitching_Data_2021)[14] <- 'HR_per_FB'
Pitching_Data[]<-lapply(Pitching_Data,gsub,pattern="%",fixed=TRUE,replacement="")
Pitching_Data_2021[]<-lapply(Pitching_Data_2021,gsub,pattern="%",fixed=TRUE,replacement="")
Pitching_Data[, c(2:21)] <- sapply(Pitching_Data[, c(2:21)], as.numeric)
Pitching_Data_2021[, c(2:21)] <- sapply(Pitching_Data_2021[, c(2:21)], as.numeric) 

#variable mutation
Pitching_Data <- Pitching_Data %>%
  mutate(LOB_pct = LOB_pct/100,
         GB_pct = GB_pct/100
         )

Pitching_Data_2021 <- Pitching_Data_2021 %>%
  mutate(LOB_pct = LOB_pct/100,
         GB_pct = GB_pct/100
         )

#relief pitching stats
relief_pitching_stats <- read.csv("~/GitHub/baseball model/relief pitching stats.csv")
colnames(relief_pitching_stats)[1] <- 'Team'

#variable creation
relief_pitching_stats <- relief_pitching_stats %>%
  mutate(games_pitched = IP / 9,
         total_ER = games_pitched * ERA)

relief <- relief_pitching_stats %>%
            group_by(League) %>%
            dplyr :: summarise(
            total_inn = sum(IP),
            runs_per_inn = sum(total_ER) / sum(total_inn)
            )

relief_avg <- mean(relief$runs_per_inn)

#steamer projection
steamer_pitching <- read.csv("~/GitHub/baseball model/steamerpitching2022_final.csv", header=TRUE)
colnames(steamer_pitching)[1] <- 'Name'
steamer_pitching$Team <- sub("^$", "FA", steamer_pitching$Team)

#lm
lm_df <- Batting_Data %>%
  select(c(hit_per_run, BB_rate, K_rate, HR_rate, ISO, BABIP, AVG, OBP, SLG))
lm_model <- lm(hit_per_run~., data = lm_df)
step <- ols_step_all_possible(lm_model)
output <- as.data.frame(cbind(step$predictors, step$adjr))
colnames(output) <- c('Predictors', 'Adj_r2')
output <- output[order(-Adj_r2),]
summary(lm_model)

lm_df_2 <- Batting_Data %>%
  select (c(hit_per_run, BB_rate, ISO, BABIP))
lm_model_2 <- lm(hit_per_run~., data = lm_df_2)          
summary(lm_model_2)
coef <- lm_model_2$coefficients
#hit_per_run = 2.5708 - 6.6440 * BB_rate - 3.2686 * ISO + 1.2421 * BABIP

#create final expected runs
Final_Offense <- Batting_Data_2021 %>%
  mutate(exp_hit_per_run = coef[1] + (coef[2] * BB_rate) + (coef[3] * ISO) + (coef[4] * BABIP),
         exp_runs = H /exp_hit_per_run,
         run_diff = exp_runs - R)
Final_Offense <- Final_Offense %>%
  select(Team, exp_hit_per_run, exp_runs, R, run_diff)


#lm pitching
lm_df <- Pitching_Data %>%
  select(c(ERA, K_per_9, BB_per_9, HR_per_9, BABIP, GB_pct, HR_per_FB))
lm_model <- lm(ERA~., data = lm_df)
step <- ols_step_all_possible(lm_model)
output <- as.data.frame(cbind(step$predictors, step$adjr))
colnames(output) <- c('Predictors', 'Adj_r2')
output <- output[order(-Adj_r2),]
summary(lm_model)

lm_df_2 <- Pitching_Data%>%
  select(c(ERA, K_per_9, BB_per_9, HR_per_9, BABIP))
lm_model_2 <- lm(ERA~., data = lm_df_2)
summary(lm_model_2)
coef2 <- lm_model_2$coef
#ERA = -1.05469 - 0.23055 * K_per_9 + 0.40402 * BB_per_9 + 1.56898 * HR_per_9 + 13.91693 * BABIP
  
#earned runs analysis
#total_runs / earned_runs = 1.086492
UE_runs <- mean(by_year$yearly_avg)

reg_ER <- lm(RA/ER~E, data = earned_run_data)
summary(reg_ER)
#no real correlation between unearned runs and errors

Pitching_Data_2021 <- Pitching_Data_2021 %>%
  mutate(exp_ERA = coef2[1] + (coef2[2] * K_per_9) + (coef2[3] * BB_per_9) + (coef2[4] * HR_per_9) + (coef2[5] * BABIP),
         exp_earned_runs_allowed = (exp_ERA * (IP/9)) + (((162*9) - IP) * relief_avg),
         total_runs_allowed = exp_earned_runs_allowed * UE_runs  
         )

#final pitching
Final_Pitching <- Pitching_Data_2021 %>%
  select(c(Team, exp_ERA, exp_earned_runs_allowed, total_runs_allowed))
Final_Pitching <- Final_Pitching %>%
  left_join(Std_2021, by = 'Team') %>%
  select(c(Team, exp_ERA, exp_earned_runs_allowed, total_runs_allowed, RA))
Final_Pitching <- Final_Pitching %>%
  mutate(runs_allowed_diff = total_runs_allowed - RA)


RA_2021 <- Std_2021 %>%
  select(Team, RA)

#2021 final pythagorean wins
Py_wins_2021 <- merge(Final_Pitching, Final_Offense, by = 'Team')
Py_wins_2021 <- Py_wins_2021 %>%
  mutate(exp_win_pct = ((exp_runs - total_runs_allowed) * coef_RD[2]) + coef_RD[1],
         exp_wins = 162 * exp_win_pct)

act_wins <- Std_2021 %>%
  select(c(Team, W))

Py_wins_2021 <- Py_wins_2021 %>%
  left_join(act_wins, by = "Team")

Py_wins_2021 <- Py_wins_2021 %>%
  mutate(win_diff = round(exp_wins - W, digits = 1))  


#steamer stats
#hit_per_run = 2.5708 - 6.6440 * BB_rate - 3.2686 * ISO + 1.2421 * BABIP
#ERA = -1.05469 - 0.23055 * K_per_9 + 0.40402 * BB_per_9 + 1.56898 * HR_per_9 + 13.91693 * BABIP
gm = 9 
runs_allowed = steamer_pitching %>%
      group_by(Team) %>%
      dplyr :: summarise(sum_ER = sum(ERA/gm * IP), sumIP = sum(IP))

runs_allowed <- runs_allowed %>%
  dplyr :: mutate(relief_innings = (162*9) - sumIP,
           relief_runs = ((162*9 - sumIP) * relief_avg),
           total_ER = sum_ER + relief_runs,
           total_runs_allowed = total_ER * UE_runs)

runs_allowed <- merge(runs_allowed, RA_2021, by = 'Team')

#compare WAR this year to last
war <- 9
WAR_2022_hitting <- steamer_hitting %>%
  group_by(Team) %>%
  dplyr :: summarise(total_WAR_2022 = sum(WAR))

WAR_2022_hitting <- left_join(WAR_2022_hitting, Batting_Data_2021, by = 'Team') %>%
  select(c(Team, total_WAR_2022, WAR)) %>%
  mutate(WAR_diff = total_WAR_2022 - WAR,
         runs_scored_diff = WAR_diff * war)

WAR_2022_pitching <- steamer_pitching %>%
  group_by(Team) %>%
  dplyr :: summarise (total_WAR_2022 = sum(WAR))

Pitching_Data_2021_starters <- read.csv("~/GitHub/baseball model/Pitching_Data_2021_starters.csv")

summarize_WAR_pitching <- Pitching_Data_2021_starters %>%
  group_by(Team) %>%
  dplyr :: summarise (total_WAR_2021 = sum(WAR))

WAR_2022_pitching <- left_join(WAR_2022_pitching, summarize_WAR_pitching, by = 'Team') %>%
  select(c(Team, total_WAR_2022, total_WAR_2021)) %>%
  mutate(WAR_diff = total_WAR_2022 - total_WAR_2021,
         runs_surr_diff = -WAR_diff * war)

#add 2022 adjustments to data
WAR_adjustment <- left_join(WAR_2022_pitching, WAR_2022_hitting, by = 'Team') %>%
  select(c(Team, runs_scored_diff, runs_surr_diff))

final_prediction <- merge(WAR_adjustment, Py_wins_2021, by = 'Team') %>%
  select(c(Team, total_runs_allowed, exp_runs, runs_scored_diff, runs_surr_diff, W)) %>%
  mutate (final_runs_scored = exp_runs + runs_scored_diff,
          final_runs_surr = total_runs_allowed + runs_surr_diff,
          exp_win_pct = (final_runs_scored - final_runs_surr) * coef_RD[2] + .500,
          exp_wins = round(162 * exp_win_pct, digits = 1),
          win_diff = exp_wins - W)

#import wynn data
wynn_over_under <- read.csv("~/GitHub/baseball model/wynn_over_under.csv")
colnames(wynn_over_under)[1] <- 'Team'

#combine model with wynn markets
bets <- left_join(final_prediction, wynn_over_under, by = 'Team') %>%
  select(c(Team, exp_wins, Over_Under)) %>%
  mutate(pct_diff = ((exp_wins - Over_Under) / Over_Under) * 100,
         diff = (exp_wins - Over_Under),
         win_pct = exp_wins / 162)

setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/daily_betting")
write.csv(bets, 'over_under_wins.csv', row.names = FALSE)
write.csv(Std_2021, 'Standings.csv', row.names = FALSE)
write.csv(final_prediction, 'final_prediction.csv', row.names = FALSE)
