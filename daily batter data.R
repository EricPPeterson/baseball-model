library (baseballr)

daily_batter_2021 <- daily_batter_bref('2021-03-26', '2021-09-30')
daily_batter_2021 <- separate(data = daily_batter_2021, col = Team, into = c("Team1", "Team2"), sep = "\\,")
