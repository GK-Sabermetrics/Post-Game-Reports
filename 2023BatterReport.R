# Batter Report
# Clear golobal environment
rm(list = ls())
# ctrl + shift + R creates a section

# Packages ----
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, 
               stringr, tidyr,tidyverse,sportyR,scales,
               ggpubr, clipr)

# Scrape MER Data ----
#load dataset from game
#data = read.csv("~/Downloads/4-14-23-ETSU.csv")

library(googlesheets4)

data = read_sheet("https://docs.google.com/spreadsheets/d/1Ed5V2s_2yv_dde2W-HpRZ48wwYLEPmegfHIyqmaq_xk/edit?usp=sharing")

#data = read.csv(file.choose())
#Filter to just our batters
#MER = filter(data, BatterTeam =='MER_BEA')

bl = unique(data$Batter)

Bat = filter(data, Batter == 'Stewart, Samuel')
Bat$Count = paste(Bat$Balls, Bat$Strikes, sep = "-")
table = data.frame(Bat$PitchNo, Bat$Count, Bat$TaggedPitchType, floor(Bat$RelSpeed), Bat$PitchCall,
                   Bat$KorBB, Bat$TaggedHitType, Bat$PlayResult, round(Bat$ExitSpeed), round(Bat$Angle, digits = 2), round(Bat$Distance))
colnames(table) <- c('PitchNo','Count','Pitch Type','Velo','Pitch Call',
                     'AB Result','Hit Type','Play Result','Exit Speed','LA','Distance')
table[table=="Undefined"] <- ""
cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A')
BatPlot = ggplot(data = Bat, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType),) +
  xlim(-3.5,3.5) + ylim(-1,5) + labs(color = "") +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
  geom_point(size = 6, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = waiver(), labels = c('Fastball' = 'FB','TwoSeamFastBall' = '2SFB','Sinker' = 'SI' ,'Cutter' = 'CU', 'Splitter' = 'SP', 'ChangeUp' = "CH",'Curveball' = 'CB','Slider' = 'SL')) +
  geom_text(data = Bat, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))
BatPlot
ggsave('Batter.png', plot = BatPlot, path = '/Users/garrettkemp/Desktop', 
       dpi = 500,width = 5, height = 4, units = c('in') ,limitsize = TRUE, bg = 'transparent')
PA <- length(which(Bat$Count == "0-0"))
Hits = length(which(Bat$PlayResult %in% c("Single", "Double", "Triple", "HomeRun")))
XBH = length(which(Bat$PlayResult %in% c("Double", "Triple", "HomeRun")))
HR = length(which(Bat$PlayResult %in% c("HomeRun")))
BB = length(which(Bat$KorBB %in% c("Walk")))
K = length(which(Bat$KorBB %in% c("Strikeout")))
t = data.frame(PA, Hits, XBH, HR, BB, K)
#AVG = format(round((Hits/AB), digits=2), nsmall = 3)

write_clip(table, col.names = FALSE)

write_clip(t, col.names = FALSE)

bl


quartz(width=8,height=6,pointsize=12,dpi=100)
BatPlot
savePlot("clipboard", )

write_clip(BatPlot)


