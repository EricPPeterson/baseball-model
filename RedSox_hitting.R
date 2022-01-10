#RedSox Hitting Statistics

stats_2021 <- daily_batter_bref('2021-03-20', '2021-10-04')
stats_2020 <- daily_batter_bref('2020-03-20', '2020-10-04')
stats_2019 <- daily_batter_bref('2019-03-20', '2019-10-04')
stats_2018 <- daily_batter_bref('2018-03-20', '2018-10-04')
stats_2017 <- daily_batter_bref('2017-03-20', '2017-10-04')
stats_total <- rbind(stats_2021, stats_2020, stats_2019, stats_2018, stats_2017)

RedSox_2021 <- stats_2021 %>%
  filter(Team == 'Boston')

RedSox_total <- stats_total %>%
  filter(Team == 'Boston')

#starting 3b
Raffy_Devers <- RedSox_total %>%
  filter(Name == 'Rafael Devers')
#starting DH
JD_Martinez <- RedSox_total %>%
  filter(Name == 'J.D. Martinez')
#starting LF
Alex_Verdugo <- RedSox_total %>%
  filter(Name == 'Alex Verdugo')
#starting SS
Xander_Bogaerts <- RedSox_total %>%
  filter(Name == 'Xander Bogaerts')
#Starting CF
Kike_Hernandez <- RedSox_total %>%
  filter(Name == 'Enrique Hernandez')
#Starting RF
Jackie_Bradley_Jr <- RedSox_total %>%
  filter(Name == 'Jackie Bradley Jr.')
#starting C
Christian_Vazquez <- RedSox_total %>%
  filter(Name == 'Christian Vazquez')
#starting 1b
Bobby_Dalbec <- RedSox_total %>%
  filter(Name == 'Bobby Dalbec')
#starting 2b
Christian_Arroyo <- RedSox_total %>%
  filter(Name == 'Christian Arroyo')
#backup catcher
Kevin_Plawecki <- RedSox_total %>%
  filter(Name == 'Kevin Plawecki')
#backup OF
Franchy_Cordero <- RedSox_total %>%
  filter(Name == 'Franchy Cordero')
#backup IF/OF
Danny_Santana <- RedSox_total %>%
  filter(Name == 'Danny Santana')
#backup OF
Jarren_Duran <- RedSox_total %>%
  filter(Name == 'Jarren_Duran')
#backup OF
Jonathan_Arauz <- RedSox_total %>%
  filter(Name == 'Jonathan Arauz')


#graphics Raffy Devers
p5_OBP <- ggplot(data = Raffy_Devers, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Raffy Devers, OBP by Season")
p5_BA <- ggplot(data = Raffy_Devers, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Raffy Devers, BA by Season")
p5_SLG <- ggplot(data = Raffy_Devers, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Raffy Devers, SLG by Season")
p5_OPS <- ggplot(data = Raffy_Devers, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Raffy Devers, OPS by Season")

ggarrange(p5_OBP, p5_BA, p5_SLG, p5_OPS,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#graphics JD Martinez
pDH_OBP <- ggplot(data = JD_Martinez, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "JD Martinez, OBP by Season")
pDH_BA <- ggplot(data = JD_Martinez, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "JD Martinez, BA by Season")
pDH_SLG <- ggplot(data = JD_Martinez, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "JD Martinez, SLG by Season")
pDH_OPS <- ggplot(data = JD_Martinez, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "JD Martinez, OPS by Season")

ggarrange(pDH_OBP, pDH_BA, pDH_SLG, pDH_OPS,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#graphics Alex Verdugo
p9_OBP <- ggplot(data = Alex_Verdugo, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Alex Verdugo, OBP by Season")
p9_BA <- ggplot(data = Alex_Verdugo, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Alex Verdugo, BA by Season")
p9_SLG <- ggplot(data = Alex_Verdugo, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Alex Verdugo, SLG by Season")
p9_OPS <- ggplot(data = Alex_Verdugo, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Alex Verdugo, OPS by Season")

ggarrange(p9_OBP, p9_BA, p9_SLG, p9_OPS,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#graphics Xander Bogaerts
p6_OBP <- ggplot(data = Xander_Bogaerts, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Xander Bogaerts, OBP by Season")
p6_BA <- ggplot(data = Xander_Bogaerts, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Xander Bogaerts, BA by Season")
p6_SLG <- ggplot(data = Xander_Bogaerts, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Xander Bogaerts, SLG by Season")
p6_OPS <- ggplot(data = Xander_Bogaerts, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Xander Bogaerts, OPS by Season")

ggarrange(p6_OBP, p6_BA, p6_SLG, p6_OPS,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

