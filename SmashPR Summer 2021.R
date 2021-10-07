library(tidyverse)
library(googlesheets4)
options(scipen = 999)

#Setup

read_sheet("https://docs.google.com/spreadsheets/d/1swxO913EEypE9G_lq5nVEKG8Alv0Vso-S8FplZGCwoM/edit#gid=383729128", sheet = "Master")

1

Master <- read_sheet("https://docs.google.com/spreadsheets/d/1swxO913EEypE9G_lq5nVEKG8Alv0Vso-S8FplZGCwoM/edit#gid=383729128", sheet = "Master")

WeightedPointsSheet <- read_sheet("https://docs.google.com/spreadsheets/d/1swxO913EEypE9G_lq5nVEKG8Alv0Vso-S8FplZGCwoM/edit#gid=954342246", sheet = "R Weighted Points")

head(Master)
attach(Master)

#Defining Filters

Acestar <- filter(Master, Player == "Acestar")
Alondite<- filter(Master, Player == "Alondite")
Butter <- filter(Master, Player == "Butter")
ChrisBCream <- filter(Master, Player == "ChrisBCream")
Darkholme <- filter(Master, Player == "Darkholme")
Feffle <- filter(Master, Player == "Feffle")
FredFredBurger <- filter(Master, Player == "FredFredBurger")
Glight <- filter(Master, Player == "Glight")
JDB <- filter(Master, Player == "JDB")
Kain <- filter(Master, Player == "Kain")
Kaiyedy <- filter(Master, Player == "Kaiyedy")
Mitch <- filter(Master, Player == "Mitch")
Mspy <- filter(Master, Player == "Mspy")
NoTap <- filter(Master, Player == "NoTap")
Platypus <- filter(Master, Player == "Platypus")
Pneumatic <- filter(Master, Player == "Pneumatic")
TCW <- filter(Master, Player == "TCW")
Vult <- filter(Master, Player == "Vult")
Worry <- filter(Master, Player == "Worry")
Jin <- filter(Master, Player == "Jin")
JinxDarkholme <- filter(Master, Player %in% c("Jin", "Darkholme"))
JinxAcestar <- filter(Master, Player %in% c("Jin", "Acestar"))

LexingtonvsLouisville <- filter(Master, `Player City` %in% c("Lexington", "Louisville"))

#Start player analysis
#Tournament performance by percentile

ggplot(data = Acestar, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Acestar's Performance Over the Season"))


ggplot(data = Alondite, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Alondite's Performance Over the Season"))

ggplot(data = Butter, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Butter's Performance Over the Season"))

ggplot(data = ChrisBCream, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("ChrisBCream's Performance Over the Season"))

ggplot(data = Darkholme, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Darkholme's Performance Over the Season"))

ggplot(data = Feffle, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Feffle's Performance Over the Season"))

ggplot(data = FredFredBurger, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("FredFredBurger's Performance Over the Season"))

ggplot(data = Glight, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Glight's Performance Over the Season"))

ggplot(data = JDB, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("JDB's Performance Over the Season"))

ggplot(data = Jin, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Jin's Performance Over the Season"))


ggplot(data = Kain, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Kain's Performance Over the Season"))

ggplot(data = Kaiyedy, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(20, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Kaiyedy's Performance Over the Season"))

ggplot(data = Mitch, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Mitch's Performance Over the Season"))

ggplot(data = Mspy, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Mspy's Performance Over the Season"))

ggplot(data = NoTap, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("NoTap's Performance Over the Season"))

ggplot(data = Platypus, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Platypus's Performance Over the Season"))

ggplot(data = Pneumatic, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Pneumatic's Performance Over the Season"))

ggplot(data = TCW, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter(mapping = aes(color = Tier)) +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("TCW's Performance Over the Season"))

ggplot(data = Vult, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Vult's Performance Over the Season"))

ggplot(data = Worry, mapping = aes(x = Day, y = Percentile)) +
  geom_line(mapping = aes(color = Tier),size = 2) +
  geom_jitter() +
  ylim(50, 100) +
  scale_color_manual(values = c("Blue", "Orange", "Grey")) +
  print(ggtitle("Worry's Performance Over the Season"))

#Rivals

ggplot(data = WeightedPointsSheet , mapping = aes(x = Acestar, y = Jin)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Jin Vs Acestar Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Pneumatic, y = Darkholme)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("DarkHolme Vs Pneumatic Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Mitch, y = Butter)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Butter Vs Mitch Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = ChrisBCream)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("ChrisBCream vs Feffle Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Worry, y = Kain)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Kain Vs Worry Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Mitch, y = Pneumatic)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Pneumatic Vs Mitch Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = TCW, y = Mitch)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Mitch Vs TCW Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Kaiyedy, y = Platypus)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Platypus Vs Kaiyedy Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Butter, y = TCW)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("TCW Vs Butter Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = Worry)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Worry Vs Feffle Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Platypus, y = Feffle)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Feffle Vs Platypus Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Platypus, y = Kaiyedy)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Kaiyedy Vs Platypus Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Jin, y = Acestar)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Acestar Vs Jin Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = NA, y = Vult)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Vult Vs NA Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = Alondite)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Alondite Vs Feffle Based on Weighted Points (Rivals)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Platypus, y = Glight)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Glight Vs Platypus Based on Weighted Points (Rivals)"))


#Buddies

ggplot(data = WeightedPointsSheet , mapping = aes(x = Kain, y = Jin)) +
  geom_smooth(method = lm, se =F) +
  print(ggtitle("Jin Vs Kain Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Kain, y = Darkholme)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("DarkHolme Vs Kain Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = Butter)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Butter Vs Feffle Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Darkholme, y = ChrisBCream)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("ChrisBCream vs Darkholme Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Darkholme, y = Kain)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Kain Vs Darkholme Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = TCW, y = Pneumatic)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Pneumatic Vs TCW Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = Mitch)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Mitch Vs Feffle Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Acestar, y = Platypus)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Platypus Vs Acestar Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Pneumatic, y = TCW)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("TCW Vs Pneumatic Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Alondite, y = Worry)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Worry Vs Alondite Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Mitch, y = Feffle)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Feffle Vs Mitch Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Feffle, y = Kaiyedy)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Kaiyedy Vs Feffle Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Kaiyedy, y = Acestar)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Acestar Vs Kaiyedy Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = NA, y = Vult)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Vult Vs NA Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Glight, y = Alondite)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Alondite Vs Glight Based on Weighted Points (Buddies)"))

ggplot(data = WeightedPointsSheet , mapping = aes(x = Mitch, y = Glight)) +
  geom_smooth(method = lm, se =F) +
  ylim(0, 30) +
  print(ggtitle("Glight Vs Mitch Based on Weighted Points (Rivals)"))

#end analysis

