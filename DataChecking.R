# Quick Checking Pitch Tagging

# Packages ----
library(pacman)
pacman::p_load(tidyverse, ggthemes, rio, scales, ggpubr, clipr)
library(googlesheets4)

cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A')
bks = c('Fastball','TwoSeamFastBall','Sinker','Cutter','Splitter','ChangeUp','Curveball','Slider')
lbs = c('FB','2SFB','SI','CU','SP','CH','CB','SL')

# data ----
data = read_sheet("https://docs.google.com/spreadsheets/d/1U9C5_p1hNjin0M2Fl8dvdmHH_CkRy1O_Ep7I22XpH4E/edit?gid=216335312#gid=216335312")

#p = unique(data$Pitcher[data$PitcherTeam == "MER_BEA"]) %>% as.data.frame()

# Pitcher ----
P = filter(data, Pitcher == "Mummert, Wyatt")
subset = P[, c("RelSpeed")]
P = P[complete.cases(subset),]
# Pitch Movement
ggplot(data = P, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())

rmggplot(data = P, aes(x=RelSide, y = RelHeight, color=TaggedPitchType)) +
  labs(x = "Release Side (ft)", y = "Release Height (ft)", color = " ", title = "Release Points") + 
  xlim(-5, 5) + ylim(3, 7) +
  geom_segment(aes(x = 0, y = 3, xend = 0, yend = 7), size = 1, color = "grey55") + 
  geom_segment(aes(x = -5, y = 5, xend = 5, yend = 5), size = 1, color = "grey55") +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  geom_point(size = 2) +
  theme_bw() + 
  theme(
    legend.position = 'bottom', 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 14)
  ) +
  guides(color = guide_legend(override.aes = list(size=4)))

