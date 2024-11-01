# PGR Edits

rm(list=ls())

# Packages ----
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,tidyverse,sportyR,scales,
               ggpubr,knitr)
# REdaS taken out of packages

a.data = read.csv(file = "~/Downloads/4-28-23-UNCG.csv")


# Select Pitcher ----
#Insert any name into Pitcher filter, change for every report you do
a.Pitcher = filter(a.data, Pitcher == 'Cosper, Colton')
# Create Datasets ----
a.CoreData = select(a.Pitcher, 'PitchNo','Pitcher','BatterSide','Inning',
                    'TaggedPitchType','PitchCall','TaggedHitType','PlayResult',
                    'RelSpeed','SpinRate','Tilt','RelHeight','RelSide','Extension',
                    'InducedVertBreak','HorzBreak','PlateLocHeight','PlateLocSide',
                    'ExitSpeed','Angle','Distance','Balls','Strikes')


b.BIP = as.data.frame(filter(a.CoreData, PitchCall == 'InPlay'))
g.x <- c(-.75,.75,.75,-.75,-.75)
g.z <- c(1.65,1.65,3.65,3.65,1.65)
#store strikezone in a dataframe for spread summary
g.strike.zone <- data.frame(g.x,g.z)

cols = c('Fastball' = '#d22d49', 'ChangeUp' = '#1dbe3a', 'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed')

g.BIP = ggplot() +
  geom_point(data = b.BIP, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size=6) +
  geom_path(data = g.strike.zone, aes(x=g.x, y=g.z),size = 1.5) + coord_equal() +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'none') +
  #ggtitle('BIP Summary') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(-3,3) +
  ylim(.5,4.5) +
  scale_color_manual(values = cols) +
  labs(col='Pitch') +
  geom_text(data = b.BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())


g.BIP

ggplot() +
  geom_path(data = g.strike.zone, aes(x=g.x, y=g.z),size = 1.5) + coord_equal() +
  geom_point(data = b.BIP, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size=6) +
  geom_text(data = b.BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'none')






# from TESTPGR.RMD

g.BIP = ggplot() +
  geom_point(data = b.BIP, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size=6) +
  geom_path(data = g.strike.zone, aes(x=g.x, y=g.z),size = 1.5) + coord_equal() +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = c(.10,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('BIP Summary', subtitle = a.CoreData$Pitcher) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  #xlim(-3,3) +
  #ylim(.5,4.5) +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  labs(col='Pitch') +
  geom_text(data = b.BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())

g.BIP

table = matrix(NA,nrow = 7, ncol = 14)

colnames(table) = c('Num','Use','Max','Avg','Spin','Tilt','HB','IVB','StkCall','StkPct','BallCall','BIP','SM','SMPct')

c.RHB = as.data.frame(filter(a.CoreData, BatterSide == 'Right' ))
c.LHB = as.data.frame(filter(a.CoreData, BatterSide == 'Left'))
#plot for vs RHB

# Really nice PItch locations chart from GIt HUB *TRACKMAN IS FROM PITCHER POV HERE .553333333
ggplot(data = MER, aes(x = PlateLocSide, y = PlateLocHeight, colour = PitchCall),) +
  xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location: Pitcher POV") +
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
  geom_point(size = 3, na.rm = TRUE) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 12), axis.title = element_blank())

# This one would be hitter POV

ggplot(data = MER, aes(x = PlateLocSide*-1, y = PlateLocHeight, colour = PitchCall),) +
  xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location: Hitter POV") +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.5, xend = 0.708, yend = 0.5), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.5, xend = -0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.15, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_point(size = 3, na.rm = TRUE) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())




# BIP chart from TESTPGR.RMD

ggplot() +
  geom_path(data = g.strike.zone, aes(x=g.x, y=g.z),size = 1.5) + coord_equal() +
  geom_point(data = b.BIP, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size=7) +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  geom_text(data = b.BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  scale_color_manual(values = cols) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'none')


if (1 == 3) 2 else 3

data.frame(1:5)%>%filter(X1.5 == 5)


MER = read.csv("/Users/garrettkemp/Documents/R-Files/Shiny APP/test.csv")

filter(MER, PitchCall %in% c('StrikeCalled', 'BallCalled'))%>%head(3)


matrix(c(1,2,3,11,12,13), ncol = 3, nrow = 2, byrow = T,
       dimnames = list(c("row a", 'row b'),
                       c('Ca','Cb','Cc')))

table(filter(MER, TaggedPitchType == "Fastball"))

length(filter(MER, TaggedPitchType == Fastball))

table(MER$TaggedPitchType)

