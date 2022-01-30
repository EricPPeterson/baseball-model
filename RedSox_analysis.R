#RedSox Analysis
library(tidyverse)
library(baseballr)
library(baseballDB)
library(ggpubr)

stats_2021 <- daily_batter_bref('2021-03-20', '2021-10-04')
stats_2020 <- daily_batter_bref('2020-03-20', '2020-10-04')
stats_2019 <- daily_batter_bref('2019-03-20', '2019-10-04')
stats_2018 <- daily_batter_bref('2018-03-20', '2018-10-04')
stats_2017 <- daily_batter_bref('2017-03-20', '2017-10-04')
stats_total <- rbind(stats_2021, stats_2020, stats_2019, stats_2018, stats_2017)

#free agent tracker
stats_total$Team[stats_total$Name == 'Jackie Bradley Jr.'] <- 'Boston'
stats_total$Team[stats_total$Name == 'Kike Hernandez'] <- 'Boston'
stats_total$Team[stats_total$Name == 'Alex Verdugo'] <- 'Boston'
stats_total$Team[stats_total$Name == 'Christian Arroyo'] <- 'Boston'
stats_total$Team[stats_total$Name == 'Kyle Schwarber'] <- 'Boston'


#hitting
RedSox_2021 <- stats_total %>%
  filter(Team == 'Boston', season == 2021)

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
       title = 'Alex Verdugo OBP') 
p9_BA <- ggplot(data = Alex_Verdugo, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Alex Verdugo, BA by Season")
p9_SLG <- ggplot(data = Alex_Verdugo, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Alex Verdugo, SLG by Season")
p9_OPS <- ggplot(data = Alex_Verdugo, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Alex Verdugo, OPS by Season")

p9_AB <- ggplot(data = Alex_Verdugo, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Alex Verdugo, PA/G')
ggarrange(p9_OBP, p9_BA, p9_SLG, p9_OPS, p9_AB,
          labels = c("A", "B", "C","D", "E"),
          ncol = 3, nrow = 2)


#graphics Xander Bogaerts
p6_OBP <- ggplot(data = Xander_Bogaerts, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Xander Bogaerts, OBP")
p6_BA <- ggplot(data = Xander_Bogaerts, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Xander Bogaerts, BA")
p6_SLG <- ggplot(data = Xander_Bogaerts, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Xander Bogaerts, SLG")
p6_OPS <- ggplot(data = Xander_Bogaerts, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Xander Bogaerts, OPS")
p6_AB <- ggplot(data = Xander_Bogaerts, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
      title = 'Xander Bogaerts, PA/G')
ggarrange(p6_OBP, p6_BA, p6_SLG, p6_OPS, p6_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)

#graphics Kike Hernandez
p8_OBP <- ggplot(data = Kike_Hernandez, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Kike Hernandez, OBP")
p8_BA <- ggplot(data = Kike_Hernandez, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Kike Hernandez, BA")
p8_SLG <- ggplot(data = Kike_Hernandez, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Kike Hernandez, SLG")
p8_OPS <- ggplot(data = Kike_Hernandez, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Kike Hernandez, OPS")
p8_AB <- ggplot(data = Kike_Hernandez, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Kike Hernandez, PA/G')
ggarrange(p8_OBP, p8_BA, p8_SLG, p8_OPS, p8_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)


#graphics Jackie Bradley Jr.
p7_OBP <- ggplot(data = Jackie_Bradley_Jr, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Jackie Bradley Jr, OBP")
p7_BA <- ggplot(data = Jackie_Bradley_Jr, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Jackie Bradley Jr, BA")
p7_SLG <- ggplot(data = Jackie_Bradley_Jr, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Jackie Bradley Jr, SLG")
p7_OPS <- ggplot(data = Jackie_Bradley_Jr, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Jackie Bradley Jr, OPS")
p7_AB <- ggplot(data = Jackie_Bradley_Jr, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Jackie Bradley Jr, PA/G')
ggarrange(p7_OBP, p7_BA, p7_SLG, p7_OPS, p7_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)


#graphics Christian Vazquez
p2_OBP <- ggplot(data = Christian_Vazquez, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Christian Vazquez, OBP")
p2_BA <- ggplot(data = Christian_Vazquez, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Christian Vazquez, BA")
p2_SLG <- ggplot(data = Christian_Vazquez, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Christian Vazquez, SLG")
p2_OPS <- ggplot(data = Christian_Vazquez, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Christian Vazquez, OPS")
p2_AB <- ggplot(data = Christian_Vazquez, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Christian Vazquez, PA/G')
ggarrange(p2_OBP, p2_BA, p2_SLG, p2_OPS, p2_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)

#graphics Bobby Dalbec
p3_OBP <- ggplot(data = Bobby_Dalbec, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Bobby Dalbec, OBP")
p3_BA <- ggplot(data = Bobby_Dalbec, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Bobby Dalbec, BA")
p3_SLG <- ggplot(data = Bobby_Dalbec, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Bobby Dalbec, SLG")
p3_OPS <- ggplot(data = Bobby_Dalbec, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Bobby Dalbec, OPS")
p3_AB <- ggplot(data = Bobby_Dalbec, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Bobby Dalbec, PA/G')
ggarrange(p3_OBP, p3_BA, p3_SLG, p3_OPS, p3_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)

#graphics Christian Arroyo
p2BU_OBP <- ggplot(data = Christian_Arroyo, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Christian Arroyo, OBP")
p2BU_BA <- ggplot(data = Christian_Arroyo, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Christian Arroyo, BA")
p2BU_SLG <- ggplot(data = Christian_Arroyo, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Christian Arroyo, SLG")
p2BU_OPS <- ggplot(data = Christian_Arroyo, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Christian Arroyo, OPS")
p2BU_AB <- ggplot(data = Christian_Arroyo, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Christian Arroyo, PA/G')
ggarrange(p2BU_OBP, p2BU_BA, p2BU_SLG, p2BU_OPS, p2BU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)

#graphics Kevin Plawecki
p7BU_OBP <- ggplot(data = Kevin_Plawecki, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Kevin Plawecki, OBP")
p7BU_BA <- ggplot(data = Kevin_Plawecki, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Kevin Plawecki, BA")
p7BU_SLG <- ggplot(data = Kevin_Plawecki, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Kevin Plawecki, SLG")
p7BU_OPS <- ggplot(data = Kevin_Plawecki, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Kevin Plawecki, OPS")
p7BU_AB <- ggplot(data = Kevin_Plawecki, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = 'AB', y = 'Season',    
       title = 'Kevin Plawecki, PA/G')
ggarrange(p7BU_OBP, p7BU_BA, p7BU_SLG, p7BU_OPS, p7BU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)



#batting statistics
total_AB <- sum(RedSox_2021$AB)
mean_OBP <- (sum(RedSox_2021$H) + sum(RedSox_2021$BB) + sum(RedSox_2021$HBP)) / sum(RedSox_2021$PA)

#Open questions for offense

#Who will play 2B?
#Who will play RF (Jackie Bradley Jr.)?

#Free Agents
#Kyle Schwarber


#pitching
pitch_2021 <- daily_pitcher_bref('2021-03-20', '2021-10-04')
pitch_2020 <- daily_pitcher_bref('2020-03-20', '2020-10-04')
pitch_2019 <- daily_pitcher_bref('2019-03-20', '2019-10-04')
pitch_2018 <- daily_pitcher_bref('2018-03-20', '2018-10-04')
pitch_2017 <- daily_pitcher_bref('2017-03-20', '2017-10-04')
pitch_total <- rbind(pitch_2021, pitch_2020, pitch_2019, pitch_2018, pitch_2017)




BOS_pitching <- read.csv("~/GitHub/baseball model/BOS pitching.csv")
BOS_starters <- BOS_pitching[1:5,]
total_inn <- sum(BOS_starters$IP)
total_inn/(162*9)
total_ER <- sum(BOS_starters$ER)
avg_run_per_inning_relief <- 0.461139
added_ER <- ((9*162) - total_inn) * avg_run_per_inning_relief
totalR_BOS <- (added_ER + total_ER) * 1.086492
#total runs allowed estimate = 729

#final record estimate

