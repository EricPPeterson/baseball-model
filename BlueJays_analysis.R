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
stats_total$Team[stats_total$Name == 'Teoscar Hernandez'] <- 'Toronto'
stats_total$Team[stats_total$Name == 'Randal Grichuk'] <- 'Toronto'
stats_total$Team[stats_total$Name == 'George Springer'] <- 'Toronto'


#hitting
BlueJays_2021 <- stats_total %>%
  filter(Team == 'Toronto', season == 2021)

BlueJays_total <- stats_total %>%
  filter(Team == 'Toronto')

#starting 1b
Vlad_Guerrero <- BlueJays_total %>%
  filter(Name == 'Vladimir Guerrero Jr.')
#starting ss
Bo_Bichette <- BlueJays_total %>%
  filter(Name == 'Bo Bichette')
#starting rf/lf
Teoscar_Hernandez <- BlueJays_total %>%
  filter(Name == 'Teoscar Hernandez')
#starting cf/rf
Randal_Grichuk <- BlueJays_total %>%
  filter(Name == 'Randal Grichuk')
#starting lf
Lourdes_Gurriel <- BlueJays_total %>%
  filter(Name == 'Lourdes Gurriel Jr.')
#backup OF
George_Springer <- BlueJays_total %>%
  filter(Name == 'George Springer')
#starting 3b
Cavan_Biggio <- BlueJays_total %>%
  filter(Name == 'Cavan Biggio')
#backup 3b
Santiago_Espinal <- BlueJays_total %>%
  filter(Name == 'Santiago Espinal')
#backup C
Reese_Mcguire <- BlueJays_total %>%
  filter(Name == 'Reese McGuire')
#backup C
Danny_Jansen <- BlueJays_total %>%
  filter(Name == 'Danny Jansen')
#backup C
Alejandro_Kirk <- BlueJays_total %>%
  filter(Name == 'Alejandro Kirk')

#graphics Vlad Guerrero Jr.
p3_OBP <- ggplot(data = Vlad_Guerrero, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Vlad Guerrero Jr., OBP by Season")
p3_BA <- ggplot(data = Vlad_Guerrero, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Vlad Guerrero Jr., BA by Season")
p3_SLG <- ggplot(data = Vlad_Guerrero, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Vlad Guerrero Jr., SLG by Season")
p3_OPS <- ggplot(data = Vlad_Guerrero, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Vlad Guerrero Jr., OPS by Season")
p3_AB <- ggplot(data = Vlad_Guerrero, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Vlad Guerrero Jr., PA/G by Season")

ggarrange(p3_OBP, p3_BA, p3_SLG, p3_OPS, p3_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Bo Bichette
p6_OBP <- ggplot(data = Bo_Bichette, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Bo Bichette, OBP by Season")
p6_BA <- ggplot(data = Bo_Bichette, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Bo Bichette, BA by Season")
p6_SLG <- ggplot(data = Bo_Bichette, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Bo Bichette, SLG by Season")
p6_OPS <- ggplot(data = Bo_Bichette, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Bo Bichette, OPS by Season")
p6_AB <- ggplot(data = Bo_Bichette, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Bo Bichette, PA/G by Season")

ggarrange(p6_OBP, p6_BA, p6_SLG, p6_OPS, p6_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Teoscar Hernandez
p7_OBP <- ggplot(data = Teoscar_Hernandez, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Teoscar Hernandez, OBP by Season")
p7_BA <- ggplot(data = Teoscar_Hernandez, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Teoscar Hernandez, BA by Season")
p7_SLG <- ggplot(data = Teoscar_Hernandez, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Teoscar Hernandez, SLG by Season")
p7_OPS <- ggplot(data = Teoscar_Hernandez, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Teoscar Hernandez, OPS by Season")
p7_AB <- ggplot(data = Teoscar_Hernandez, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Teoscar Hernandez, PA/G by Season")

ggarrange(p7_OBP, p7_BA, p7_SLG, p7_OPS, p7_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Randal Grichuk
p8_OBP <- ggplot(data = Randal_Grichuk, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Randall Grichuk, OBP by Season")
p8_BA <- ggplot(data = Randal_Grichuk, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Randal Grichuk, BA by Season")
p8_SLG <- ggplot(data = Randal_Grichuk, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Randal Grichuk, SLG by Season")
p8_OPS <- ggplot(data = Randal_Grichuk, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Randal Grichuk, OPS by Season")
p8_AB <- ggplot(data = Randal_Grichuk, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Randal Grichuk, PA/G by Season")

ggarrange(p8_OBP, p8_BA, p8_SLG, p8_OPS, p8_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Lourdes Gurriel
p9_OBP <- ggplot(data = Lourdes_Gurriel, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Lourdes Gurriel, OBP by Season")
p9_BA <- ggplot(data = Lourdes_Gurriel, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Lourdes Gurriel, BA by Season")
p9_SLG <- ggplot(data = Lourdes_Gurriel, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Lourdes Gurriel, SLG by Season")
p9_OPS <- ggplot(data = Lourdes_Gurriel, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Lourdes Gurriel, OPS by Season")
p9_AB <- ggplot(data = Lourdes_Gurriel, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Lourdes Gurriel, PA/G by Season")

ggarrange(p9_OBP, p9_BA, p9_SLG, p9_OPS, p9_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics George Springer
pBUOF_OBP <- ggplot(data = George_Springer, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "George Springer, OBP by Season")
pBUOF_BA <- ggplot(data = George_Springer, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "George Springer, BA by Season")
pBUOF_SLG <- ggplot(data = George_Springer, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "George Springer, SLG by Season")
pBUOF_OPS <- ggplot(data = George_Springer, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "George Springer, OPS by Season")
pBUOF_AB <- ggplot(data = George_Springer, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "George Springer, PA/G by Season")

ggarrange(pBUOF_OBP, pBUOF_BA, pBUOF_SLG, pBUOF_OPS, pBUOF_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Cavan Biggio
p5_OBP <- ggplot(data = Cavan_Biggio, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Cavan Biggio, OBP by Season")
p5_BA <- ggplot(data = Cavan_Biggio, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Cavan Biggio, BA by Season")
p5_SLG <- ggplot(data = Cavan_Biggio, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Cavan Biggio, SLG by Season")
p5_OPS <- ggplot(data = Cavan_Biggio, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Cavan Biggio, OPS by Season")
p5_AB <- ggplot(data = Cavan_Biggio, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Cavan Biggio, PA/G by Season")

ggarrange(p5_OBP, p5_BA, p5_SLG, p5_OPS, p5_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Santiago Espinal
pBU5_OBP <- ggplot(data = Santiago_Espinal, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Santiago Espinal, OBP by Season")
pBU5_BA <- ggplot(data = Santiago_Espinal, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Santiago Espinal, BA by Season")
pBU5_SLG <- ggplot(data = Santiago_Espinal, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Santiago Espinal, SLG by Season")
pBU5_OPS <- ggplot(data = Santiago_Espinal, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Santiago Espinal, OPS by Season")
pBU5_AB <- ggplot(data = Santiago_Espinal, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Santiago Espinal, PA/G by Season")

ggarrange(pBU5_OBP, pBU5_BA, pBU5_SLG, pBU5_OPS, pBU5_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Reese McGuire
p2_OBP <- ggplot(data = Reese_Mcguire, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Reese McGuire, OBP by Season")
p2_BA <- ggplot(data = Reese_Mcguire, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Reese McGuire, BA by Season")
p2_SLG <- ggplot(data = Reese_Mcguire, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Reese McGuire, SLG by Season")
p2_OPS <- ggplot(data = Reese_Mcguire, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Reese McGuire, OPS by Season")
p2_AB <- ggplot(data = Reese_Mcguire, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Reese McGuire, PA/G by Season")

ggarrange(p2_OBP, p2_BA, p2_SLG, p2_OPS, p2_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#graphics Danny Jansen
pBU2_OBP <- ggplot(data = Danny_Jansen, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "Danny Jansen, OBP by Season")
pBU2_BA <- ggplot(data = Danny_Jansen, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Danny Jansen, BA by Season")
pBU2_SLG <- ggplot(data = Danny_Jansen, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Danny Jansen, SLG by Season")
pBU2_OPS <- ggplot(data = Danny_Jansen, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Danny Jansen, OPS by Season")
pBU2_AB <- ggplot(data = Danny_Jansen, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Danny Jansen, PA/G by Season")


ggarrange(pBU2_OBP, pBU2_BA, pBU2_SLG, pBU2_OPS, pBU2_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)


#graphics ALejandro Kirk
pBU2_OBP <- ggplot(data = Alejandro_Kirk, aes(x = season, y = OBP, group = 1)) + geom_line() + 
  labs(x = "OBP", y = "Season", 
       title = "ALejandro Kirk, OBP by Season")
pBU2_BA <- ggplot(data = Alejandro_Kirk, aes(x = season, y = BA, group = 1)) + geom_line() + 
  labs(x = "BA", y = "Season", 
       title = "Alejandro Kirk, BA by Season")
pBU2_SLG <- ggplot(data = Alejandro_Kirk, aes(x = season, y = SLG, group = 1)) + geom_line() + 
  labs(x = "SLG", y = "Season", 
       title = "Alejandro Kirk, SLG by Season")
pBU2_OPS <- ggplot(data = Alejandro_Kirk, aes(x = season, y = OPS, group = 1)) + geom_line() + 
  labs(x = "OPS", y = "Season", 
       title = "Alejandro Kirk, OPS by Season")
pBU2_AB <- ggplot(data = Alejandro_Kirk, aes(x = season, y = PA/G, group = 1)) + geom_line() + 
  labs(x = "AB/G", y = "Season", 
       title = "Alejandro Kirk, PA/G by Season")


ggarrange(pBU2_OBP, pBU2_BA, pBU2_SLG, pBU2_OPS, pBU2_AB,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#batting statistics
total_AB <- sum(BlueJays_2021$AB)
mean_OBP <- (sum(BlueJays_2021$H) + sum(BlueJays_2021$BB) + sum(BlueJays_2021$HBP)) / sum(BlueJays_2021$PA)


#Free Agents
#signed SP Kevin Gausman

#pitching
pitch_2021 <- daily_pitcher_bref('2021-03-20', '2021-10-04')
pitch_2020 <- daily_pitcher_bref('2020-03-20', '2020-10-04')
pitch_2019 <- daily_pitcher_bref('2019-03-20', '2019-10-04')
pitch_2018 <- daily_pitcher_bref('2018-03-20', '2018-10-04')
pitch_2017 <- daily_pitcher_bref('2017-03-20', '2017-10-04')
pitch_total <- rbind(pitch_2021, pitch_2020, pitch_2019, pitch_2018, pitch_2017)

TOR_pitching <- read.csv("~/GitHub/baseball model/Toronto Pitching.csv")
TOR_starters <- TOR_pitching[1:6,]
total_inn <- sum(TOR_starters$IP)
total_inn/(162*9)
total_ER <- sum(TOR_starters$ER)
avg_run_per_inning_relief <- 0.461139
added_ER <- ((9*162) - total_inn) * avg_run_per_inning_relief
totalR_TOR <- (added_ER + total_ER) * 1.086492
#total runs allowed estimate = 708

#final record estimate



#hitting outstanding questions
#who will replace marcus semien
#who will get the majority of PA at the C position?

