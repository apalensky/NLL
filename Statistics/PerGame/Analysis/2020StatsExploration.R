# Alexander Palensky

rm(list = ls())


library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)

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
         PIMp60 = (60 / TOF) * PIM, DVp60 = (60 / TOF) *Devittes,
         
         ATO_ratio = A / TO,
         PTO_ratio = Points / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


avgPointsp60 = mean(df2020$Pointsp60, na.rm = TRUE)
avgPointsp60
sdPointsp60 = sd(df2020$Pointsp60, na.rm = TRUE)
sdPointsp60

avgDVp60 = mean(df2020$DVp60, na.rm = TRUE)
avgDVp60
sdDVp60 = sd(df2020$DVp60, na.rm = TRUE)
sdDVp60

avgPTO_ratio = mean(df2020$PTO_ratio, na.rm = TRUE)
avgPTO_ratio
sdDPTO_ratio = sd(df2020$PTO_ratio, na.rm = TRUE)
sdDPTO_ratio


dfPTO_ratio = df2020 %>% 
  filter(Pointsp60 > avgPointsp60 + sdPointsp60, TO > 0) %>%
  group_by(Name, Team) %>%
  summarize(G, A, Points, TO, TOFPG, PTO_ratio, DVp60, Pointsp60)



plot1 = ggplot(data = dfPTO_ratio, aes(x=PTO_ratio, y=DVp60, color=TOFPG, alpha = 0.5)) + geom_point() + 
  geom_text(aes(label = Name, hjust = -.1)) + labs(title = "Points-to-Turnover Ratio vs. Devittes per game") +
  scale_x_continuous(limits = c(.6,6)) + scale_y_continuous(limits = c(-8,10.5)) +
  theme(legend.position = "bottom", axis.title.x = element_text("PTO_ratio"),axis.title.y = element_text("DVp60"),
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_colour_distiller(palette = "YlOrRd")

plot1


plot2 = ggplot(data = dfPTO_ratio, aes(x=TOFPG, y=DVp60, color=PTO_ratio, alpha = 0.5)) + geom_point() + 
  geom_text(aes(label = Name, hjust = -.1)) + labs(title = "Time on Floor per game vs. Devittes per game") +
  scale_x_continuous(limits = c(9.8,26)) + scale_y_continuous(limits = c(-8,10.5)) +
  theme(legend.position = "bottom", legend.title = element_text("PTO ratio"), plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis()

plot2

plot3 = ggplot(data = dfPTO_ratio, aes(x=TOFPG, y=PTO_ratio, color=DVp60, alpha = 0.5)) + geom_point() + 
  geom_text(aes(label = Name, hjust = -.1)) + labs(title = "Time on Floor per game vs. Points-to-Turnover Ratio") +
  scale_x_continuous(limits = c(9.8,26)) + scale_y_continuous(limits = c(.59,5.1)) +
  theme(legend.position = "bottom", legend.title = element_text("DVp60"), plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis()

plot3

plot4 = ggplot(data = dfPTO_ratio, aes(x=TOFPG, y=Pointsp60, color=PTO_ratio, alpha = 0.5)) + geom_point() + 
  geom_text(aes(label = Name, hjust = -.1)) + labs(title = "Time on Floor per game vs. Points per 60 minutes") +
  scale_x_continuous(limits = c(9.8,26)) + scale_y_continuous(limits = c(10,17.7)) +
  theme(legend.position = "bottom", legend.title = element_text("PTO ratio"), plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis()

plot4
