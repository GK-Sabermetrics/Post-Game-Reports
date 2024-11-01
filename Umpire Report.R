# upmire reports
rm(list = ls())

library(ggplot2)
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, 
               stringr, tidyr,tidyverse, scales, ggpubr)

#load dataset from game
#data = read.csv(file.choose())%>%filter(PitchCall %in% c('BallCalled', 'StrikeCalled') )

game = read.csv(file.choose())

data = game%>%filter(PitchCall %in% c('BallCalled', 'StrikeCalled'))

MissedStrikes = filter(data, PlateLocHeight < '1.4' | PlateLocHeight > 3.6 | 
                         PlateLocSide < -0.95 | PlateLocSide > 0.95)%>%
  filter(PitchCall == 'StrikeCalled')

MissedBalls = filter(data, between(PlateLocHeight, 1.5, 3.5))%>%
  filter(between(PlateLocSide, -0.83, 0.83))%>%
  filter(PitchCall == "BallCalled")

# New Graph ====

# g.SZ

g.SZ = ggplot() +
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
  xlim(-3,3) + ylim(0,5) + labs(col='Pitch Call') + theme(legend.position = 'none') +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  geom_point(data = data, aes(x=PlateLocSide,y=PlateLocHeight,color = PitchCall), size= 5 ) +
  #geom_text(data = data, aes(label = PitchNo, x = PlateLocSide, y = PlateLocHeight), size = 2, color = 'black') +
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c('BallCalled' = '#00d1ed', 'StrikeCalled'= '#d12e49'))

# g.MB

g.MB = ggplot() +
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
  xlim(-3,3) + ylim(0,5) + labs(title = "Missed Balls") + theme(legend.position = 'none') +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  geom_point(data = MissedStrikes, aes(x=PlateLocSide,y=PlateLocHeight,color = PitchCall), size= 5 ) +
  #geom_text(data = data, aes(label = PitchNo, x = PlateLocSide, y = PlateLocHeight), size = 2, color = 'black') +
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c('BallCalled' = '#00d1ed', 'StrikeCalled'= '#d12e49'))


# g.MS

g.MS = ggplot() +
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
  xlim(-3,3) + ylim(0,5) + labs(title = "Missed Strikes") + theme(legend.position = 'none') +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  geom_point(data = MissedBalls, aes(x=PlateLocSide,y=PlateLocHeight,color = PitchCall), size= 5 ) +
  #geom_text(data = data, aes(label = PitchNo, x = PlateLocSide, y = PlateLocHeight), size = 2, color = 'black') +
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c('BallCalled' = '#00d1ed', 'StrikeCalled'= '#d12e49'))


ggarrange(g.SZ, g.MB, g.MS, common.legend = TRUE)




# Old Graph ====

# to tell it to draw strike zone for Spread Summary
g.x <- c(-.75,.75,.75,-.75,-.75)
g.z <- c(1.65,1.65,3.65,3.65,1.65)
#store strikezone in a dataframe for spread summary
g.strike.zone <- data.frame(g.x,g.z)

ggplot() +
  geom_point(data = d.data, aes(x=PlateLocSide,y=PlateLocHeight,color=PitchCall), size=4) +
  geom_path(data = g.strike.zone, aes(x=g.x, y=g.z),size = 1.5) + coord_equal() +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = c(.10,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('Umpire Report') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlim(-4.5,4.5) +
  ylim(0,6) +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  labs(col='Pitch Call') +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())

ggsave('UMP.png', path = '/Users/garrettkemp/Pictures/RPlots/Game', 
       dpi = 500,width = 6, height = 5, units = c('in') ,limitsize = TRUE, bg = 'transparent')

