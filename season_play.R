library(dplyr)
library(tidyverse)

#Every team now plays 19 games against each of 4 opponents within its division (76 games), as well as 6 games each against 
#4 opponents and 7 games against each of the other 6 opponents from other divisions within its own league (66 games).
#Interleague play will feature AL East vs. NL Central, AL Central vs. NL West, and AL West vs. NL East.

#import data from model
over_under_wins <- read.csv("~/GitHub/baseball model/daily_betting/over_under_wins2.csv")
Standings <- read.csv("~/GitHub/baseball model/daily_betting/Standings")
season_play <- left_join(over_under_wins, Standings, by = 'Team') %>%
  select(c('Team', 'win_pct', 'Division'))
season_play <- season_play[order(season_play$Division),]
season_play$wins <- 0
season_play$loss <- 0


#season play
#log5 model
log5 <- function(A,B){
  Pa = (A - (A * B)) / (A + B - (2 * A * B))
  return(Pa)
}

games <- function(X = season_play){
  for(i in 1:nrow(X)){
    s1.letter = strsplit(X[i,3], split = "")[[1]]
    s1.division <- X[i,3]
    if(i == nrow(X)){
      return(X)}

    for(j in 1:nrow(X)){
      if(j > i){
        s2.letter <- strsplit(X[j,3], split= "") [[1]]
        s2.division = X[j,3]
       
        if((s1.division == 'AL East' & s2.division == 'NL Cent') | (s1.division == 'NL Cent' & s2.division == 'AL East')) {
            n <- 4
            
            win_pct_1 <- X[i,2]
            win_pct_2 <- X[j,2]
      
            Pa <- log5(win_pct_1,win_pct_2)
            
            rand_num <- runif(n,0,1)
          
            for(k in 1:length(rand_num)){
              if(rand_num[k] < Pa){
                X[i,4] <- X[i,4] + 1
                X[j,5] <- X[j,5] + 1
              } else {
                X[i,5] <- X[i,5] + 1
                X[j,4] <- X[j,4] +1
            }
          }
          
          } else if ((s1.division == 'AL Cent' & s2.division == 'NL West') | (s1.division == 'NL West' & s2.division == 'AL Cent')) {
            n <- 4
            
            win_pct_1 <- X[i,2]
            win_pct_2 <- X[j,2]
            
            Pa <- log5(win_pct_1,win_pct_2)
            
            rand_num <- runif(n,0,1)
            
            for(k in 1:length(rand_num)){
              if(rand_num[k] < Pa){
                X[i,4] <- X[i,4] + 1
                X[j,5] <- X[j,5] + 1
              } else {
                X[i,5] <- X[i,5] + 1
                X[j,4] <- X[j,4] +1
              }
            } 
            
            
          } else if ((s1.division == 'AL West' & s2.division == 'NL East') || (s1.division == 'NL East' & s2.division == 'AL West')) {
            n <- 4
            
            win_pct_1 <- X[i,2]
            win_pct_2 <- X[j,2]
            
            Pa <- log5(win_pct_1,win_pct_2)
            
            rand_num <- runif(n,0,1)
            
            for(k in 1:length(rand_num)){          
              if(rand_num[k] < Pa){
                X[i,4] <- X[i,4] + 1
                X[j,5] <- X[j,5] + 1
              } else {
                X[i,5] <- X[i,5] + 1
                X[j,4] <- X[j,4] + 1
              }
            }
              
          } else if (s1.division == s2.division) {
            n <- 19
            
            win_pct_1 <- X[i,2]
            win_pct_2 <- X[j,2]
            
            Pa <- log5(win_pct_1,win_pct_2)
            
            rand_num <- runif(n,0,1)
            
            for(k in 1:length(rand_num)){
              if(rand_num[k] < Pa){
                X[i,4] <- X[i,4] + 1
                X[j,5] <- X[j,5] + 1
              } else {
                X[i,5] <- X[i,5] + 1
                X[j,4] <- X[j,4] +1
              }
            }
            } else if((s1.letter[1] == s2.letter[1]) && s1.letter[4] != s2.letter[4]) {
              
                n <- 6
            
                win_pct_1 <- X[i,2]
                win_pct_2 <- X[j,2]
              
                Pa <- log5(win_pct_1,win_pct_2)
              
                rand_num <- runif(n,0,1)
              
                for(k in 1:length(rand_num)){
                  if(rand_num[k] < Pa){
                    X[i,4] <- X[i,4] + 1
                    X[j,5] <- X[j,5] + 1
                  } else {
                    X[i,5] <- X[i,5] + 1
                    X[j,4] <- X[j,4] +1
                }
              }
            }
      } else {
        next
      }
    }
  }
  return(X)
}



output <- games()

#construct one-thousand seasons
library(glue)
thousand_seasons <- data.frame(matrix(nrow = 30, ncol = 1))
colnames(thousand_seasons) <- c('Team')
thousand_seasons$Team <- season_play$Team
thousand_seasons$Division <- season_play$Division

#do loop 10k times
for(k in 1:10000){
  output <- games()
  wins <- output$wins
  thousand_seasons <- cbind(thousand_seasons,wins)
  colnames(thousand_seasons)[k+2] <- glue('year_{k}')
  print(k)
}


library(ggplot2)
library(eply)
library(ggeasy)
#loop through data and make histograms of wins
# Basic histogram
teams <- thousand_seasons$Team
myplots <- vector('list', 5)

for(i in 1:length(teams)){
  message(i)
  data <- thousand_seasons %>%
    dplyr :: filter(Team == teams[i])
  # keep the first column 
  names <-  data[,1]
  # Transpose everything other than the first column
  hist_data <- as.data.frame(as.matrix(t(data[c(3:10002)])))
  colnames(hist_data) <- names
  
  myplots[[i]] <- local({
    i <- i
    #histogram
    p <- ggplot(hist_data, aes(x = noquote(hist_data[,1]))) + 
      geom_histogram(binwidth = 1) + 
      scale_x_continuous(name = teams[i]) +
      scale_y_continuous(name = 'Count') + 
      ggtitle('Total Wins') +
      ggeasy :: easy_center_title()
    print(p)
  })
}


#calculate winners of division over 10k seasons
AL_East <- thousand_seasons %>%
  dplyr :: filter(Division == 'AL East')

df_ALEast <- data.frame(AL_East$Team)
df_ALEast$Division_Win <- 0


for(i in 3:ncol(AL_East)){
  if(AL_East[1,i] == max(AL_East[,i])){
    df_ALEast[1,2] <- df_ALEast[1,2] + 1
    } else if(AL_East[2,i] == max(AL_East[,i])){
      df_ALEast[2,2] <- df_ALEast[2,2] + 1
    } else if(AL_East[3,i] == max(AL_East[,i])){
      df_ALEast[3,2] <- df_ALEast[3,2] + 1 
    } else if(AL_East[4,i] == max(AL_East[,i])){
      df_ALEast[4,2] <- df_ALEast[4,2] + 1
    } else if(AL_East[5,i] == max(AL_East[5,i])){
      df_ALEast[5,2] <- df_ALEast[5,2] + 1
    }
}


#AL Central
AL_Cent <- thousand_seasons %>%
  dplyr :: filter(Division == 'AL Cent')

df_ALCent <- data.frame(AL_Cent$Team)
df_ALCent$Division_Win <- 0


for(i in 3:ncol(AL_Cent)){
  if(AL_Cent[1,i] == max(AL_Cent[,i])){
    df_ALCent[1,2] <- df_ALCent[1,2] + 1
  } else if(AL_Cent[2,i] == max(AL_Cent[,i])){
    df_ALCent[2,2] <- df_ALCent[2,2] + 1
  } else if(AL_Cent[3,i] == max(AL_Cent[,i])){
    df_ALCent[3,2] <- df_ALCent[3,2] + 1 
  } else if(AL_Cent[4,i] == max(AL_Cent[,i])){
    df_ALCent[4,2] <- df_ALCent[4,2] + 1
  } else if(AL_Cent[5,i] == max(AL_Cent[5,i])){
    df_ALCent[5,2] <- df_ALCent[5,2] + 1
  }
}


#AL West
AL_West <- thousand_seasons %>%
  dplyr :: filter(Division == 'AL West')

df_ALWest <- data.frame(AL_West$Team)
df_ALWest$Division_Win <- 0


for(i in 3:ncol(AL_West)){
  if(AL_West[1,i] == max(AL_West[,i])){
    df_ALWest[1,2] <- df_ALWest[1,2] + 1
  } else if(AL_West[2,i] == max(AL_West[,i])){
    df_ALWest[2,2] <- df_ALWest[2,2] + 1
  } else if(AL_West[3,i] == max(AL_West[,i])){
    df_ALWest[3,2] <- df_ALWest[3,2] + 1 
  } else if(AL_West[4,i] == max(AL_West[,i])){
    df_ALWest[4,2] <- df_ALWest[4,2] + 1
  } else if(AL_West[5,i] == max(AL_West[5,i])){
    df_ALWest[5,2] <- df_ALWest[5,2] + 1
  }
}


#NL East
NL_East <- thousand_seasons %>%
  dplyr :: filter(Division == 'NL East')

df_NLEast <- data.frame(NL_East$Team)
df_NLEast$Division_Win <- 0


for(i in 3:ncol(NL_East)){
  if(NL_East[1,i] == max(NL_East[,i])){
    df_NLEast[1,2] <- df_NLEast[1,2] + 1
  } else if(NL_East[2,i] == max(NL_East[,i])){
    df_NLEast[2,2] <- df_NLEast[2,2] + 1
  } else if(NL_East[3,i] == max(NL_East[,i])){
    df_NLEast[3,2] <- df_NLEast[3,2] + 1 
  } else if(NL_East[4,i] == max(NL_East[,i])){
    df_NLEast[4,2] <- df_NLEast[4,2] + 1
  } else if(NL_East[5,i] == max(NL_East[5,i])){
    df_NLEast[5,2] <- df_NLEast[5,2] + 1
  }
}


#NL Cent
NL_Cent <- thousand_seasons %>%
  dplyr :: filter(Division == 'NL Cent')

df_NLCent <- data.frame(NL_Cent$Team)
df_NLCent$Division_Win <- 0


for(i in 3:ncol(NL_Cent)){
  if(NL_Cent[1,i] == max(NL_Cent[,i])){
    df_NLCent[1,2] <- df_NLCent[1,2] + 1
  } else if(NL_Cent[2,i] == max(NL_Cent[,i])){
    df_NLCent[2,2] <- df_NLCent[2,2] + 1
  } else if(NL_Cent[3,i] == max(NL_Cent[,i])){
    df_NLCent[3,2] <- df_NLCent[3,2] + 1 
  } else if(NL_Cent[4,i] == max(NL_Cent[,i])){
    df_NLCent[4,2] <- df_NLCent[4,2] + 1
  } else if(NL_Cent[5,i] == max(NL_Cent[5,i])){
    df_NLCent[5,2] <- df_NLCent[5,2] + 1
  }
}


#NL West
NL_West <- thousand_seasons %>%
  dplyr :: filter(Division == 'NL West')

df_NLWest <- data.frame(NL_West$Team)
df_NLWest$Division_Win <- 0


for(i in 3:ncol(NL_West)){
  if(NL_West[1,i] == max(NL_West[,i])){
    df_NLWest[1,2] <- df_NLWest[1,2] + 1
  } else if(NL_West[2,i] == max(NL_West[,i])){
    df_NLWest[2,2] <- df_NLWest[2,2] + 1
  } else if(NL_West[3,i] == max(NL_West[,i])){
    df_NLWest[3,2] <- df_NLWest[3,2] + 1 
  } else if(NL_West[4,i] == max(NL_West[,i])){
    df_NLWest[4,2] <- df_NLWest[4,2] + 1
  } else if(NL_West[5,i] == max(NL_West[5,i])){
    df_NLWest[5,2] <- df_NLWest[5,2] + 1
  }
}


#barplots of div winners
#AL East
p <- ggplot(data = df_ALEast, aes(x = AL_East.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p

#AL Cent
p <- ggplot(data = df_ALCent, aes(x = AL_Cent.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p

#AL West
p <- ggplot(data = df_ALWest, aes(x = AL_West.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p

#NL East
p <- ggplot(data = df_NLEast, aes(x = NL_East.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p

#NL Cent
p <- ggplot(data = df_NLCent, aes(x = NL_Cent.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p

#NL West
p <- ggplot(data = df_NLWest, aes(x = NL_West.Team, y = Division_Win)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label= Division_Win / sum(Division_Win)), vjust=-0.3, size=3.5) +
  theme_minimal()
p
