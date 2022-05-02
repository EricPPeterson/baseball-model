#import libraries needed
library(dplyr)
library(stringr)
library(rvest)
library(Dict)

home_teams <- data.frame(matrix(ncol = 1, nrow = 0))
away_teams <- data.frame(matrix(ncol = 1, nrow = 0))
url <- 'https://www.cbssports.com/mlb/schedule/20220425/'
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


