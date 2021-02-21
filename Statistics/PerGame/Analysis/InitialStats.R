# Alexander Palensky

#############################################################################################
# Derived per game
# Average and distribution of floor time
# Goals / 60 minutes
# Statistics relative to league, then examine relative to team
# LB / 60 minutes
# Derived per season
# Assists - turnovers ratio 
# SOG, SOFF, goals
# adjusted/true? shooting percent
# goals / SOG
# Goals above replacement (GAR)

#############################################################################################

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

# Dhane Smith
dfDhane2020 = df %>% 
  filter((Date >= "2019-11-29" & Date <= "2020-03-08"), (Name == 'D.Smith'))

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

#df2020path = 'C:/Users/alexa/OneDrive/Documents/Data Science/Projects/NLL/PerGameStats/2020StatsPerGameCombined.csv'
#write.csv(df2020, df2020path, row.names = FALSE)


# 2019
df2019 = df %>%
  filter((Date >= "2018-12-15" & Date <= "2019-04-27")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )

#df2019path = 'C:/Users/alexa/OneDrive/Documents/Data Science/Projects/NLL/PerGameStats/2019StatsPerGameCombined.csv'
#write.csv(df2019, df2019path, row.names = FALSE)


df2019Playoffs = df %>%
  filter((Date >= "2019-05-03" & Date <= "2019-05-25")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )

#df2019playoffspath = 'C:/Users/alexa/OneDrive/Documents/Data Science/Projects/NLL/PerGameStats/2019PlayoffsStatsPerGameCombined.csv'
#write.csv(df2019Playoffs, df2019playoffspath, row.names = FALSE)


# 2018
df2018 = df %>%
  filter((Date >= "2017-12-08" & Date <= "2018-04-29")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2017
df2017 = df %>%
  filter((Date >= "2016-12-29" & Date <= "2017-04-29")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2016
df2016 = df %>%
  filter((Date >= "2016-01-01" & Date <= "2016-05-01")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2015
df2015 = df %>%
  filter((Date >= "2015-01-02" & Date <= "2015-05-02")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2014
df2014 = df %>%
  filter((Date >= "2013-12-28" & Date <= "2014-04-26")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2013
df2013 = df %>%
  filter((Date >= "2013-01-05" & Date <= "2013-04-20")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2012
df2012 = df %>%
  filter((Date >= "2012-01-08" & Date <= "2012-04-28")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2011
df2011 = df %>%
  filter((Date >= "2011-01-08" & Date <= "2011-04-23")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


# 2010
df2010 = df %>%
  filter((Date >= "2010-01-08" & Date <= "2010-04-24")) %>%
  group_by(Name, Team) %>%
  summarize(Games = n(), Points = sum(Points), G = sum(G), A = sum(A), PM = sum(PM), PIM = sum(PIM), S = sum(S),
            SOFF = sum(SOFF), LB = sum(LB), TO = sum(TO), CT = sum(CT), FO_W = sum(FO_W),
            FO = sum(FO)) %>%
  mutate(PointsPG = Points / Games, GPG = G / Games, APG = A / Games, PMPG = PM / Games,
         PIMPG = PIM / Games, SPG = S / Games, SOFFPG = SOFF / Games, LBPG = LB / Games, 
         TOPG = TO / Games, CTPG = CT / Games, FO_WPG = FO_W / Games, FOPG = FO / Games,
         
         ATO_ration = A / TO,
         FOpercent = FO_W / FO,
         
         ShootingPct = G / (S + SOFF),
         AdjShootingPct = G / S
  )


#############################################################################################

# Visualizations

qplot(Points,         # the "x" column
      data = df2020,          # our data frame
      geom = "histogram", # the "geometry" like to plot
      binwidth = 2)

ggplot(data = df2020) + 
  geom_point(mapping = aes(x = G, y = A))
