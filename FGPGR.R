# Full Game report

rm(list=ls())

rm()


# Packages ----------------------------------------------------------------
library(ggplot2)
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,tidyverse, REdaS, sportyR, scales, ggpubr)


# Strike Zone Selectors ----------------------------------------------------------

g.x <- c(-.75,.75,.75,-.75,-.75)
g.z <- c(1.65,1.65,3.65,3.65,1.65)
#store strikezone in a dataframe for spread summary
g.strike.zone <- data.frame(g.x,g.z)
# Count Data -----------------------------------------------------------------------

a.data = read.csv(file.choose())

#a.MER = filter(a.data, BatterTeam == 'MER_BEA')
b.OPP = filter(a.data, PitcherTeam == 'FLO_RAT')

b.OPP = filter(b.OPP, Pitcher == 'Brown, Rese')


# Counts -----------------------------------------------------------------------
c00 = filter(b.OPP, Balls == '0', Strikes == '0')
c00 = select(c00, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c01 = filter(b.OPP, Balls == '0', Strikes == '1')
c01 = select(c01, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c02 = filter(b.OPP, Balls == '0', Strikes == '2')
c02 = select(c02, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c10 = filter(b.OPP, Balls == '1', Strikes == '0')
c10 = select(c10, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c11 = filter(b.OPP, Balls == '1', Strikes == '1')
c11 = select(c11, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c12 = filter(b.OPP, Balls == '1', Strikes == '2')
c12 = select(c12, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c20 = filter(b.OPP, Balls == '2', Strikes == '0')
c20 = select(c20, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c21 = filter(b.OPP, Balls == '2', Strikes == '1')
c21 = select(c21, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c22 = filter(b.OPP, Balls == '2', Strikes == '2')
c22 = select(c22, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c30 = filter(b.OPP, Balls == '3', Strikes == '0')
c30 = select(c30, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
                 'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c31 = filter(b.OPP, Balls == '3', Strikes == '1')
c31 = select(c31, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')
c32 = filter(b.OPP, Balls == '3', Strikes == '2')
c32 = select(c32, 'PitchNo','Inning','Pitcher','BatterSide','Outs','Balls','Strikes',
             'TaggedPitchType','PitchCall','PlateLocHeight','PlateLocSide')

# Graph Counts -----------------------------------------------------------------------

# 0-0 Count ----------------------------------------------------------------
g00 = ggplot() +
  geom_point(data = c00, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('0-0 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 0-1 Count ----------------------------------------------------------------
g01 = ggplot() +
  geom_point(data = c01, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('0-1 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 0-2 Count ----------------------------------------------------------------
g02 = ggplot() +
  geom_point(data = c02, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('0-2 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 1-0 Count ---------------------------------------------------------------
g10 = ggplot() +
  geom_point(data = c10, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('1-0 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 1-1 Count ---------------------------------------------------------------
g11 = ggplot() +
  geom_point(data = c11, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('1-1 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 1-2 Count ---------------------------------------------------------------
g12 = ggplot() +
  geom_point(data = c12, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('1-2 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 2-0 Count ---------------------------------------------------------------
g20 = ggplot() +
  geom_point(data = c20, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('2-0 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 2-1 Count ---------------------------------------------------------------
g21 = ggplot() +
  geom_point(data = c21, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('2-1 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 2-2 Count ---------------------------------------------------------------
g22 = ggplot() +
  geom_point(data = c22, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('2-2 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 3-0 Count ---------------------------------------------------------------
g30 = ggplot() +
  geom_point(data = c30, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('3-0 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 3-1 Count ---------------------------------------------------------------
g31 = ggplot() +
  geom_point(data = c31, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('3-1 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# 3-2 Count ---------------------------------------------------------------
g32 = ggplot() +
  geom_point(data = c32, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path(data = g.strike.zone,aes(x=g.x, y=g.z),size = 1.5,lineend ='round',linejoin='round') + 
  coord_equal() +
  geom_segment(aes(x =.25,y = 1.65,xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.25,y = 1.65,xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 3,xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment(aes(x =-.75,y = 2.3,xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('3-2 Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-3,3) +
  ylim(.3,5) +
  labs(col='Pitch') +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())



# Bind Plots --------------------------------------------------------------

ggarrange(g00,g10,g20,g30,g01,g11,g21,g31,g02,g12,g22,g32 + rremove("x.text"), 
          ncol = 4, nrow = 3)

ggsave('Pitches.png', path = '/Users/garrettkemp/Pictures/RPlots/Game', 
       dpi = 500,width = 12, height = 11, units = c('in') ,limitsize = TRUE, bg = 'transparent')

