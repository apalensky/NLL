# Alexander Palensky

rm(list = ls())


library(ggplot2)
library(dplyr)
library(tidyverse)

dffull <- read.csv("NLLFloorGameStats.csv")

df = dffull[2:20]

df$Date = as.Date(df$Date, "%Y-%m-%d")
df$Year = lubridate::year(df$Date)


names(df)[names(df) == "X."] = "Number"
names(df)[names(df) == "X..."] = "PM"
names(df)[names(df) == "T"]<-"TO"

df['Points'] = df['A'] + df['G']
df['Gp60'] = (60 / df['TOF']) * df['G']
df['Ap60'] = (60 / df['TOF']) * df['A']
df['LBp60'] = (60 / df['TOF']) * df['LB']


# TOF distributions

#############################################################################################

# Season summary tables


#2020
df2020 = df %>% 
  filter((Date >= "2019-11-29" & Date <= "2020-03-08")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO), TOF = sum(TOF), Devittes = (sum(Points) - sum(TO))) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         TOFPG = TOF / Games, DVPG = Devittes / Games,
         
         Pointsp60 = (60 / TOF) * Points, Gp60 = (60 / TOF) * G, Ap60 = (60 / TOF) * A,
         LBp60 = (60 / TOF) * LB, CTp60 = (60 / TOF) * CT, PMp60 = (60 / TOF) * PM,
         PIMp60 = (60 / TOF) * PIM, DVp60 = (60 / Devittes),
         
         ATO_ratio = A / TO,
         PTO_ratio = Points / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )

avgTOF = mean(df2020$TOF, na.rm = TRUE)
sdTOF = sd(df2020$TOF, na.rm = TRUE)

dfPTO_ratio = df2020 %>% 
  filter(TOF > (avgTOF-sdTOF), TO > 0, PointsPG > 1) %>%
  group_by(Name, Team) %>%
  summarize(G, A, Points, TO, PTO_ratio, TOFPG, DVPG = (Points - TO)/ Games) %>%
  filter(DVPG > 1.5)



plot1 = ggplot(data = dfPTO_ratio, aes(x=PTO_ratio, y=DVPG, color=TOFPG, alpha = 0.5)) + geom_point() + 
  geom_text(aes(label = Name, hjust = -.1)) + labs(title = "Points-to-Turnover Ratio vs. Devittes per game") +
  scale_x_continuous(limits = c(1.5,6)) + scale_y_continuous(limits = c(1.5,4)) +
  theme(legend.position = "bottom", axis.title.x = element_text("PTO_ratio"),axis.title.y = element_text("DVPG"),
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_colour_distiller(palette = "BrBG")

plot1
