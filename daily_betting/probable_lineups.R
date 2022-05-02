#probable lineups

url_tomorrow <- 'https://www.rotowire.com/baseball/daily-lineups.php?date=tomorrow'
url_today <- 'https://www.rotowire.com/baseball/daily-lineups.php'

page_tomorrow <- read_html(url_tomorrow)
page_today <- read_html(url_today)
teams_today <- read_html(url_today)
teams_tomorrow <- read_html(url_tomorrow)

lineup_tomorrow <- page_tomorrow %>% html_nodes('.lineup__player a') %>% html_text()
lineup_today <- page_today %>% html_nodes('.lineup__player a') %>% html_text()
teams_today <- page_today %>% html_nodes('lineup__abbr') %>% html_text()
teams_tomorrow <- page_tomorrow %>% html_nodes('.lineup__abbr') %>% html_text()

probables_pitching <- page_tomorrow %>% html_nodes('.lineup__player-highlight-name a') %>% html_text()
confirmed_pitching <- page_today %>% html_nodes('.lineup__player-highlight-name a') %>% html_text()

lineup_tomorrow <- data.frame(lineup_tomorrow, stringsAsFactors = FALSE)
probables_pitching <- data.frame(probables_pitching, stringsAsFactors = FALSE)
colnames(lineup_tomorrow) <- 'Probables'
colnames(probables_pitching) <- 'Probable_Pitchers'
teams_tomorrow <- data.frame(teams_tomorrow, stringsAsFactors = FALSE)

lineup_today <- data.frame(lineup_today, stringsAsFactors = FALSE)
confirmed_pitching <- data_frame(confirmed_pitching)
colnames(lineup_today) <- 'Confirmed_Lineup'
colnames(confirmed_pitching) <- 'Confirmed_Pitchers'

setwd("/Users/ericp/OneDrive/Documents/GitHub/baseball model/daily_betting")
write.csv(probables_lineup, 'probables_lineup.csv', row.names = FALSE)
write.csv(probables_pitching, 'probables_pitching.csv', row.names = FALSE)
write.csv(confirmed_roto, 'confirmed_roto.csv', row.names = FALSE)
write.csv(confirmed_roto_pitching, 'confirmed_roto_pitching', row.names = FALSE)