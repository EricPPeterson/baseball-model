#scrape daily lineups 
#scrape injury data
#scrape FA signings

library(tidyverse)
library(rvest)

#import data from model
over_under_wins <- read.csv("~/GitHub/baseball model/daily_betting/over_under_wins.csv")

#scrape lineups from baseball monster
todays_lineup <- read_html('https://baseballmonster.com/lineups.aspx')
table <- html_table(todays_lineup, header = TRUE, fill = TRUE)

#pull out table of data for all lineups
first_table <- table[[1]]
first_table

#import steamer pitching and hitting
steamer_hitting <- as.data.frame(read.csv("~/GitHub/baseball model/steamer_hitting.csv")) %>%
  dplyr::filter(AB > 50)
colnames(steamer_hitting)[1] <- 'Name'
steamer_hitting$Team <- sub("^$", "FA", steamer_hitting$Team)

steamer_pitching <- read.csv("~/GitHub/baseball model/steamer_pitching.csv") %>%
  dplyr :: filter(IP > 20)
colnames(steamer_pitching)[1] <- 'Name'
steamer_pitching$Team <- sub("^$", "FA", steamer_pitching$Team)

#merge lineups with steamer data
#filter on hitting lineups
Hitting_Lineup <- todays_lineup %>%
  filter(position != P)
#filter on pitching lineups
Pitching_Lineup <- todays_lineup %>%
  filter(position == P)

#join hitting lineup with steamer data from players
Hitting_Lineup <- left_join(Hitting_Lineup, steamer_hitting) %>%
  select(c(Name, Team, WAR))
#join pitching lineup with steamer data from players
Pitching_Lineup <- left_join(Pitching_Lineup, steamer_pitching) %>%
  select(c(Name, Team, WAR))


#add home field advantage
home_team <- function(win_prob){
  win_prob = win_prob + .04
  return(win_prob)
}

away_team <- function(win_prob){
  win_prob = win_prob - 0.04
  return(win_prob)
}

#log5 model
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}