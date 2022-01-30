#Yankees Analysis
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
stats_total$Team[stats_total$Name == 'DJ LeMahieu'] <- 'New York'
stats_total$Level[stats_total$Name == 'DJ LeMahieu'] <- 'Maj-AL'
stats_total$Team[stats_total$Name == 'Giancarlo Stanton'] <- 'New York'
stats_total$Level[stats_total$Name == 'Giancarlo Stanton'] <- 'Maj-AL'
stats_total$Team[stats_total$Name == 'Gio Urshela'] <- 'New York'
stats_total$Level[stats_total$Name == 'Giio Urshela'] <- 'Maj-AL'
stats_total$Team[stats_total$Name == 'Rougned Odor'] <- 'New York'
stats_total$Level[stats_total$Name == 'Rougned Odor'] <- 'Maj-AL'
stats_total$Team[stats_total$Name == 'Luke Voit'] <- 'New York'
stats_total$Level[stats_total$Name == 'Luke Voit'] <- 'Maj-AL'


#hitting
Yankees_2021 <- stats_total %>%
  filter(Team == 'New York', Level == 'Maj-AL', season == 2021)

Yankees_total <- stats_total %>%
  filter(Team == 'New York', Level == 'Maj-AL')

#starting 2b/3b/1b
DJ_LeMahieu <- Yankees_total %>%
  filter(Name == 'DJ LeMahieu')
#starting 1b
Aaron_Judge <- Yankees_total %>%
  filter(Name == 'Aaron Judge')
#starting DH
Giancarlo_Stanton <- Yankees_total %>%
  filter(Name == 'Giancarlo Stanton')
#starting SS/2b
Gleyber_Torres <- Yankees_total %>%
  filter(Name == 'Gleyber Torres')
#Starting CF/LF
Brett_Gardner <- Yankees_total %>%
  filter(Name == 'Brett Gardner')
#Starting SS/3b
Gio_Urshela <- Yankees_total %>%
  filter(Name == 'Gio Urshela')
#starting C
Gary_Sanchez <- Yankees_total %>%
  filter(Name == 'Gary Sanchez')
#starting 1b
Luke_Voit <- Yankees_total %>%
  filter(Name == 'Luke Voit')
#backup C
Kyle_Higashioka <- Yankees_total %>%
  filter(Name == 'Kyle Higashioka')
#backup 3b/LF
Miguel_Andujar <- Yankees_total %>%
  filter(Name == 'Miguel Andujar')
#backup 2b/OF
Tyler_Wade <- Yankees_total %>%
  filter(Name == 'Tyler Wade')
#backup CF
Aaron_Hicks <- Yankees_total %>%
  filter(Name == 'Aaron Hicks')


#graphics DJ LeMahieu
p4_OBP <- ggplot(data = DJ_LeMahieu, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "DJ LeMahieu, OBP by Season")
p4_BA <- ggplot(data = DJ_LeMahieu, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "DJ LeMahieu, BA by Season")
p4_SLG <- ggplot(data = DJ_LeMahieu, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "DJ LeMahieu, SLG by Season")
p4_OPS <- ggplot(data = DJ_LeMahieu, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "DJ LeMahieu, OPS by Season")
p4_AB <- ggplot(data = DJ_LeMahieu, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "DJ LeMahieu, PA/G by Season")

ggarrange(p4_OBP, p4_BA, p4_SLG, p4_OPS, p4_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)


#graphics Aaron Judge
p3_OBP <- ggplot(data = Aaron_Judge, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Aaron Judge, OBP by Season")
p3_BA <- ggplot(data = Aaron_Judge, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Aaron Judge, BA by Season")
p3_SLG <- ggplot(data = Aaron_Judge, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Aaron Judge, SLG by Season")
p3_OPS <- ggplot(data = Aaron_Judge, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Aaron Judge, OPS by Season")
p3_AB <- ggplot(data = Aaron_Judge, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Aaron Judge, PA/G by Season")

ggarrange(p3_OBP, p3_BA, p3_SLG, p3_OPS, p3_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Giancarlo Stanton
pDH_OBP <- ggplot(data = Giancarlo_Stanton, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Giancarlo Stanton, OBP by Season")
pDH_BA <- ggplot(data = Giancarlo_Stanton, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Giancarlo Stanton, BA by Season")
pDH_SLG <- ggplot(data = Giancarlo_Stanton, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Giancarlo Stanton, SLG by Season")
pDH_OPS <- ggplot(data = Giancarlo_Stanton, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Giancarlo Stanton, OPS by Season")
pDH_AB <- ggplot(data = Giancarlo_Stanton, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Giancarlo Stanton, PA/G by Season")

ggarrange(pDH_OBP, pDH_BA, pDH_SLG, pDH_OPS, pDH_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Gleyber Torres
p6_OBP <- ggplot(data = Gleyber_Torres, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Gleyber Torres, OBP by Season")
p6_BA <- ggplot(data = Gleyber_Torres, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Gleyber Torres, BA by Season")
p6_SLG <- ggplot(data = Gleyber_Torres, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Gleyber Torres, SLG by Season")
p6_OPS <- ggplot(data = Gleyber_Torres, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Gleyber Torres, OPS by Season")
p6_AB <- ggplot(data = Gleyber_Torres, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Gleyber Torres, PA/G by Season")

ggarrange(p6_OBP, p6_BA, p6_SLG, p6_OPS, p6_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Brett Gardner
p8_OBP <- ggplot(data = Brett_Gardner, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Brett Gardner, OBP by Season")
p8_BA <- ggplot(data = Brett_Gardner, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Brett Gardner, BA by Season")
p8_SLG <- ggplot(data = Brett_Gardner, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Brett Gardner, SLG by Season")
p8_OPS <- ggplot(data = Brett_Gardner, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Brett Gardner, OPS by Season")
p8_AB <- ggplot(data = Brett_Gardner, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Brett Gardner, PA/G by Season")

ggarrange(p8_OBP, p8_BA, p8_SLG, p8_OPS, p8_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Gio Urshela
p5_OBP <- ggplot(data = Gio_Urshela, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Gio Urshela, OBP by Season")
p5_BA <- ggplot(data = Gio_Urshela, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Gio Urshela, BA by Season")
p5_SLG <- ggplot(data = Gio_Urshela, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Gio Urshela, SLG by Season")
p5_OPS <- ggplot(data = Gio_Urshela, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Gio Urshela, OPS by Season")
p5_AB <- ggplot(data = Gio_Urshela, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Gio Urshela, PA/G by Season")

ggarrange(p5_OBP, p5_BA, p5_SLG, p5_OPS, p5_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Gary Sanchez
p2_OBP <- ggplot(data = Gary_Sanchez, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Gary Sanchez, OBP by Season")
p2_BA <- ggplot(data = Gary_Sanchez, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Gary Sanchez, BA by Season")
p2_SLG <- ggplot(data = Gary_Sanchez, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Gary Sanchez, SLG by Season")
p2_OPS <- ggplot(data = Gary_Sanchez, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Gary Sanchez, OPS by Season")
p2_AB <- ggplot(data = Gary_Sanchez, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Gary Sanchez, PA/G by Season")

ggarrange(p2_OBP, p2_BA, p2_SLG, p2_OPS, p2_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Luke Voit
p3_OBP <- ggplot(data = Luke_Voit, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Luke Voit, OBP by Season")
p3_BA <- ggplot(data = Luke_Voit, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Luke Voit, BA by Season")
p3_SLG <- ggplot(data = Luke_Voit, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Luke Voit, SLG by Season")
p3_OPS <- ggplot(data = Luke_Voit, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Luke Voit, OPS by Season")
p3_AB <- ggplot(data = Luke_Voit, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Luke Voit, PA/G by Season")

ggarrange(p3_OBP, p3_BA, p3_SLG, p3_OPS, p3_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Kyle Higashioka
pCBU_OBP <- ggplot(data = Kyle_Higashioka, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Kyle Higashioka, OBP by Season")
pCBU_BA <- ggplot(data = Kyle_Higashioka, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Kyle Higashioka, BA by Season")
pCBU_SLG <- ggplot(data = Kyle_Higashioka, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Kyle Higashioka, SLG by Season")
pCBU_OPS <- ggplot(data = Kyle_Higashioka, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Kyle Higashioka, OPS by Season")
pCBU_AB <- ggplot(data = Kyle_Higashioka, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Kyle Higashioka, PA/G by Season")

ggarrange(pCBU_OBP, pCBU_BA, pCBU_SLG, pCBU_OPS, pCBU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Miguel Andujar
p7BU_OBP <- ggplot(data = Miguel_Andujar, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Miguel Andujar, OBP by Season")
p7BU_BA <- ggplot(data = Miguel_Andujar, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Miguel Andujar, BA by Season")
p7BU_SLG <- ggplot(data = Miguel_Andujar, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Miguel Andujar, SLG by Season")
p7BU_OPS <- ggplot(data = Miguel_Andujar, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Miguel Andujar, OPS by Season")
p7BU_AB <- ggplot(data = Miguel_Andujar, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Miguel Andujar, PA/G by Season")

ggarrange(p7BU_OBP, p7BU_BA, p7BU_SLG, p7BU_OPS, p7BU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Tyler Wade
pOFBU_OBP <- ggplot(data = Tyler_Wade, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Tyler Wade, OBP by Season")
pOFBU_BA <- ggplot(data = Miguel_Andujar, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Tyler Wade, BA by Season")
pOFBU_SLG <- ggplot(data = Tyler_Wade, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Tyler Wade, SLG by Season")
pOFBU_OPS <- ggplot(data = Tyler_Wade, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Tyler Wade, OPS by Season")
pOFBU_AB <- ggplot(data = Tyler_Wade, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Tyler Wade, PA/G by Season")

ggarrange(pOFBU_OBP, pOFBU_BA, pOFBU_SLG, pOFBU_OPS, pOFBU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Aaron Hicks
p8BU_OBP <- ggplot(data = Aaron_Hicks, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Aaron Hicks, OBP by Season")
p8BU_BA <- ggplot(data = Aaron_Hicks, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Aaron Hicks, BA by Season")
p8BU_SLG <- ggplot(data = Aaron_Hicks, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Aaron Hicks, SLG by Season")
p8BU_OPS <- ggplot(data = Aaron_Hicks, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Aaron Hicks, OPS by Season")
p8BU_AB <- ggplot(data = Aaron_Hicks, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Aaron Hicks, PA/G by Season")

ggarrange(p8BU_OBP, p8BU_BA, p8BU_SLG, p8BU_OPS, p8BU_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)


#batting statistics
total_AB <- sum(Yankees_2021$AB)
mean_OBP <- (sum(Yankees_2021$H) + sum(Yankees_2021$BB) + sum(Yankees_2021$HBP)) / sum(Yankees_2021$PA)

#Open questions for offense

#Who will take Rougned Odor's ABs?
#Who will take Clint Frazier's ABs?

#Free Agents
#Brett Gardner
#Anthony Rizzo


#pitching
pitch_2021 <- daily_pitcher_bref('2021-03-20', '2021-10-04')
pitch_2020 <- daily_pitcher_bref('2020-03-20', '2020-10-04')
pitch_2019 <- daily_pitcher_bref('2019-03-20', '2019-10-04')
pitch_2018 <- daily_pitcher_bref('2018-03-20', '2018-10-04')
pitch_2017 <- daily_pitcher_bref('2017-03-20', '2017-10-04')
pitch_total <- rbind(pitch_2021, pitch_2020, pitch_2019, pitch_2018, pitch_2017)



NYY_pitching <- read.csv("~/GitHub/baseball model/NYY Pitching.csv")
NYY_starters <- NYY_pitching[1:5,]
total_inn <- sum(NYY_starters$IP)
total_inn/(162*9)
total_ER <- sum(NYY_starters$ER)
avg_run_per_inning_relief <- 0.461139
added_ER <- ((9*162) - total_inn) * avg_run_per_inning_relief
totalR_NYY <- (added_ER + total_ER) * 1.086492
#total runs allowed estimate = 722

#final record estimate
