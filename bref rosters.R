#pull 2022 teams from Baseball Reference
#set lineup with probable pitchers and standard lineup
teams <- c('ARI', 'ATL', 'BAL', 'BOS', 'CHC', 
           'CHW', 'CIN', 'CLE', 'COL', 'DET', 
           'HOU', 'KCR', 'LAA', 'LAD', 'MIA', 
           'MIL', 'MIN', 'NYM', 'NYY', 'OAK', 
           'PHI','PIT', 'SDP', 'SEA', 'SFG', 
           'STL', 'TBR', 'TEX', 'TOR', 'WSN')
league <- data.frame()

for(i in teams){
  url <- paste0('https://www.baseball-reference.com/teams/',i,'/2021-roster.shtml')
  #read url into correct format
  page <- url %>% read_html() %>% html_nodes('table')
  #turn page_probables into dataframe
  teamX <- url %>% read_html() %>% html_nodes('table') %>% .[1] %>% html_table(trim = T, header = T) %>%
    data.frame(stringsAsFactors = FALSE)
  teamX$Team <- i
  
  league <- rbind(league, teamX)
}

league <- league %>%
  filter(Uni != 'Uni')

hitters <- league %>%
  filter(Var.5 == 'Position')
pitchers <- league %>%
  filter(Var.5 == 'Pitcher')

#bref projections
url <- 'https://www.baseball-reference.com/leagues/majors/2021-projections.shtml'
page <- url %>% read_html() %>% html_nodes('table')
#turn page_probables into dataframe
projections <- url %>% read_html() %>% html_nodes('table') %>% .[1] %>% html_table(trim = T, header = T) %>%
  data.frame(stringsAsFactors = FALSE)



