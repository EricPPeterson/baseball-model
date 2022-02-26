

#import final predictions (use final_runs_scored, final_runs_surrendered for daily betting)
final_prediction<- read.csv("~/GitHub/baseball model/daily_betting/final_prediction")
adjust <- final_prediction %>%
  select(Team, final_runs_scored, final_runs_surr)
steamerbatting <- read.csv("~/GitHub/baseball model/steamerbatting.csv")
colnames(steamerbatting)[1] <- 'Name'
steamerpitching <- read.csv("~/GitHub/baseball model/steamerpitching.csv")
colnames(steamerpitching)[1] <- 'Name'
#need to import final lineups to set daily lineup
