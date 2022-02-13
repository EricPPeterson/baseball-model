library(dplyr)
library(tidyverse)

#Every team now plays 19 games against each of 4 opponents within its division (76 games), as well as 6 games each against 
#4 opponents and 7 games against each of the other 6 opponents from other divisions within its own league (66 games).

#import data from model
over_under_wins <- read.csv("~/GitHub/baseball model/daily_betting/over_under_wins.csv")
Standings <- read.csv("~/GitHub/baseball model/daily_betting/Standings")
season_play <- left_join(over_under_wins, Standings, by = 'Team') %>%
  select(c('Team', 'win_pct', 'Division'))

AL_East <- season_play %>%
  dplyr :: filter(Division == 'AL East')
AL_Cent <- season_play %>%
  dplyr :: filter(Division == 'AL Cent')
AL_West <- season_play %>%
  dplyr :: filter(Division == 'AL West')
NL_East <- season_play %>%
  dplyr :: filter(Division == 'NL East')
NL_Cent <- season_play %>%
  dplyr :: filter(Division == 'NL Cent')
NL_West <- season_play %>%
  dplyr :: filter(Division == 'NL West')

#season play
#log5 model
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}

games <- function(X){
  
  for(i in length(X))
    {
    s1.letter = strsplit(X[i,3], split = "")[[1]]
    s2.letter = strsplit(X[(i+1),3], split= "") [[1]]
    if(X[i,2] == X[(i+1),2])
      {
      n <- 19 
        } else if(s1.letter[1] == s2.letter[1] & (s1.letter[4] != s1.letter[4]) ) {
          n <- 6
        } else {
          n <- 4
            }
        }
      win_pct_1 <- X[i,2]
      win_pct_2 <- X[(i+1),2]
        Pa <- log5(win_pct_1,win_pct_2)
        Pb <- 1 - Pa
        rand_num <- runif(n,0,1)
  print(Pa)
  print(Pb)
  print(n)
  return()
  }
