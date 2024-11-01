# PGR Full Code Set

# Clear golobal environment
rm(list =ls())

# option + CMD + L collapse sections
# shift + option + CMD + L opens sections
# option + CMD + O collapse all sections
# shift + option + CMD + O opens all sections
# ctrl + shift + R creates a section

# Packages ----
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,tidyverse,sportyR,scales,
               ggpubr,knitr)
# REdaS taken out of packages

# Scrape MER Data ----
#load dataset from game

data = read.csv(file.choose())

data = read.csv("~/Downloads/4-28-23-UNCG.csv")

#Filter to just our pitchers
#a.MER = filter(a.data, PitcherTeam =='MER_BEA')

attach(data)

Pitcher[PitcherTeam == 'MER_BEA']%>%unique() 


# Select Pitcher ----
#Insert any name into Pitcher filter, change for every report you do
a.Pitcher = filter(data, Pitcher == 'Cosper, Colton')
# Create Datasets ----
a.CoreData = select(a.Pitcher, 'PitchNo','Pitcher','BatterSide','Inning',
                  'TaggedPitchType','PitchCall','TaggedHitType','PlayResult',
                  'RelSpeed','SpinRate','Tilt','RelHeight','RelSide','Extension',
                  'InducedVertBreak','HorzBreak','PlateLocHeight','PlateLocSide',
                  'ExitSpeed','Angle','Distance','Balls','Strikes')

# ONLY IF THERE IS AN NA VALUE IN CORE DATA
#a.CoreData = a.CoreData[-c(11,22,96),]

subset = a.CoreData[, c("RelSpeed")]

a.CoreData = a.CoreData[complete.cases(subset),]

#attach(a.CoreData)

mean(a.CoreData$RelSpeed)

detach(data)

# Pitches and Tilts -------------------------------------------------------
p.FB = filter(a.CoreData, TaggedPitchType == 'Fastball')
p.CH = filter(a.CoreData, TaggedPitchType == 'ChangeUp')
p.SL = filter(a.CoreData, TaggedPitchType == 'Slider')
p.SP = filter(a.CoreData, TaggedPitchType == 'Splitter')
p.CB = filter(a.CoreData, TaggedPitchType == 'Curveball')
p.SI = filter(a.CoreData, TaggedPitchType == 'Sinker')
p.CU = filter(a.CoreData, TaggedPitchType == 'Cutter')
# Tilts -------------------------------------------------------------------
ti.FB = c(p.FB$Tilt)
ti.FB = round(seconds_to_period(mean(period_to_seconds(hm(ti.FB)))))
ti.FB = expand_grid(x = c(ti.FB@hour), y = c(ti.FB@minute))
pd.tiFB = ti.FB %>% unite("z", x:y, sep = ':')
ti.CH = c(p.CH$Tilt)
ti.CH = seconds_to_period(mean(period_to_seconds(hm(ti.CH))))
ti.CH = expand_grid(x = c(ti.CH@hour), y = c(ti.CH@minute))
pd.tiCH = ti.CH %>% unite("z", x:y, sep = ':')
ti.SL = c(p.SL$Tilt)
ti.SL = seconds_to_period(mean(period_to_seconds(hm(ti.SL))))
ti.SL = expand_grid(x = c(ti.SL@hour), y = c(ti.SL@minute))
pd.tiSL = ti.SL %>% unite("z", x:y, sep = ':')
ti.SP = c(p.SP$Tilt)
ti.SP = seconds_to_period(mean(period_to_seconds(hm(ti.SP))))
ti.SP = expand_grid(x = c(ti.SP@hour), y = c(ti.SP@minute))
pd.tiSP = ti.SP %>% unite("z", x:y, sep = ':')
ti.CB = c(p.CB$Tilt)
ti.CB = seconds_to_period(mean(period_to_seconds(hm(ti.CB))))
ti.CB = expand_grid(x = c(ti.CB@hour), y = c(ti.CB@minute))
pd.tiCB = ti.CB %>% unite("z", x:y, sep = ':')
ti.SI = c(p.SI$Tilt)
ti.SI = seconds_to_period(mean(period_to_seconds(hm(ti.SI))))
ti.SI = expand_grid(x = c(ti.SI@hour), y = c(ti.SI@minute))
pd.tiSI = ti.SI %>% unite("z", x:y, sep = ':')
ti.CU = c(p.CU$Tilt)
ti.CU = seconds_to_period(mean(period_to_seconds(hm(ti.CU))))
ti.CU = expand_grid(x = c(ti.CU@hour), y = c(ti.CU@minute))
pd.tiCU = ti.CU %>% unite("z", x:y, sep = ':')
# * Pitch Data ----
# ** Fastball Data ----
pd.nFB = table(p.FB$TaggedPitchType)
pd.pFB = scales::percent(length(p.FB$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxFB = round(max(p.FB$RelSpeed))
pd.avgFB = round(mean(p.FB$RelSpeed))
pd.spnFB = round(mean(p.FB$SpinRate))
pd.hbFB =  round(mean(p.FB$HorzBreak),digits = 2)
pd.ivbFB = round(mean(p.FB$InducedVertBreak),digits = 1)
pd.stkFB = length(which(p.FB$PitchCall == 'StrikeCalled'))
pd.stkPctFB = scales::percent(pd.stkFB/length(a.CoreData$TaggedPitchType))
pd.ballFB = length(which(p.FB$PitchCall == 'BallCalled'))
pd.bipFB = length(which(p.FB$PitchCall == 'InPlay'))
pd.smFB = length(which(p.FB$PitchCall == 'StrikeSwinging'))
pd.smPctFB = scales::percent(pd.smFB/length(a.CoreData$TaggedPitchType))
# ** Change Data ----
pd.nCH = table(p.CH$TaggedPitchType)
pd.pCH = scales::percent(length(p.CH$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxCH = round(max(p.CH$RelSpeed))
pd.avgCH = round(mean(p.CH$RelSpeed))
pd.spnCH = round(mean(p.CH$SpinRate))
pd.hbCH =  round(mean(p.CH$HorzBreak),digits = 2)
pd.ivbCH = round(mean(p.CH$InducedVertBreak),digits = 1)
pd.stkCH = length(which(p.CH$PitchCall == 'StrikeCalled'))
pd.stkPctCH = scales::percent(pd.stkCH/length(a.CoreData$TaggedPitchType))
pd.ballCH = length(which(p.CH$PitchCall == 'BallCalled'))
pd.bipCH = length(which(p.CH$PitchCall == 'InPlay'))
pd.smCH = length(which(p.CH$PitchCall == 'StrikeSwinging'))
pd.smPctCH = scales::percent(pd.smCH/length(a.CoreData$TaggedPitchType))
# ** Slider Data ----
pd.nSL = table(p.SL$TaggedPitchType)
pd.pSL = scales::percent(length(p.SL$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxSL = round(max(p.SL$RelSpeed))
pd.avgSL = round(mean(p.SL$RelSpeed))
pd.spnSL = round(mean(p.SL$SpinRate))
pd.hbSL =  round(mean(p.SL$HorzBreak),digits = 2)
pd.ivbSL = round(mean(p.SL$InducedVertBreak),digits = 1)
pd.stkSL =length(which(p.SL$PitchCall == 'StrikeCalled'))
pd.stkPctSL = scales::percent(pd.stkSL/length(a.CoreData$TaggedPitchType))
pd.ballSL = length(which(p.SL$PitchCall == 'BallCalled'))
pd.bipSL = length(which(p.SL$PitchCall == 'InPlay'))
pd.smSL = length(which(p.SL$PitchCall == 'StrikeSwinging'))
pd.smPctSL = scales::percent(pd.smSL/length(a.CoreData$TaggedPitchType))
# ** Splitter Data ----
pd.nSP = table(p.SP$TaggedPitchType)
pd.pSP = scales::percent(length(p.SP$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxSP = round(max(p.SP$RelSpeed))
pd.avgSP = round(mean(p.SP$RelSpeed))
pd.spnSP = round(mean(p.SP$SpinRate))
pd.hbSP =  round(mean(p.SP$HorzBreak),digits = 2)
pd.ivbSP = round(mean(p.SP$InducedVertBreak),digits = 1)
pd.stkSP = length(which(p.SP$PitchCall == 'StrikeCalled'))
pd.stkPctSP = scales::percent(pd.stkSP/length(a.CoreData$TaggedPitchType))
pd.ballSP = length(which(p.SP$PitchCall == 'BallCalled'))
pd.bipSP = length(which(p.SP$PitchCall == 'InPlay'))
pd.smSP = length(which(p.SP$PitchCall == 'StrikeSwinging'))
pd.smPctSP = scales::percent(pd.smSP/length(a.CoreData$TaggedPitchType))
# ** Curveball Data ----
pd.nCB = table(p.CB$TaggedPitchType)
pd.pCB = scales::percent(length(p.CB$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxCB = round(max(p.CB$RelSpeed))
pd.avgCB = round(mean(p.CB$RelSpeed))
pd.spnCB = round(mean(p.CB$SpinRate))
pd.hbCB =  round(mean(p.CB$HorzBreak),digits = 2)
pd.ivbCB = round(mean(p.CB$InducedVertBreak),digits = 1)
pd.stkCB = length(which(p.CB$PitchCall == 'StrikeCalled'))
pd.stkPctCB = scales::percent(pd.stkCB/length(a.CoreData$TaggedPitchType))
pd.ballCB = length(which(p.CB$PitchCall == 'BallCalled'))
pd.bipCB = length(which(p.CB$PitchCall == 'InPlay'))
pd.smCB = length(which(p.CB$PitchCall == 'StrikeSwinging'))
pd.smPctCB = scales::percent(pd.smCB/length(a.CoreData$TaggedPitchType))
# ** Sinker Data ----
pd.nSI = table(p.SI$TaggedPitchType)
pd.pSI = scales::percent(length(p.SI$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxSI = round(max(p.SI$RelSpeed))
pd.avgSI = round(mean(p.SI$RelSpeed))
pd.spnSI = round(mean(p.SI$SpinRate))
pd.hbSI =  round(mean(p.SI$HorzBreak),digits = 2)
pd.ivbSI = round(mean(p.SI$InducedVertBreak),digits = 1)
pd.stkSI = length(which(p.SI$PitchCall == 'StrikeCalled'))
pd.stkPctSI = scales::percent(pd.stkSI/length(a.CoreData$TaggedPitchType))
pd.ballSI = length(which(p.SI$PitchCall == 'BallCalled'))
pd.bipSI = length(which(p.SI$PitchCall == 'InPlay'))
pd.smSI = length(which(p.SI$PitchCall == 'StrikeSwinging'))
pd.smPctSI = scales::percent(pd.smSI/length(a.CoreData$TaggedPitchType))
# ** Cutter Data ----
pd.nCU = table(p.CU$TaggedPitchType)
pd.pCU = scales::percent(length(p.CU$TaggedPitchType)/length(a.CoreData$TaggedPitchType))
pd.maxCU = round(max(p.CU$RelSpeed))
pd.avgCU = round(mean(p.CU$RelSpeed))
pd.spnCU = round(mean(p.CU$SpinRate))
pd.hbCU =  round(mean(p.CU$HorzBreak), digits = 2)
pd.ivbCU = round(mean(p.CU$InducedVertBreak), digits = 1)
pd.stkCU = length(which(p.CU$PitchCall == 'StrikeCalled'))
pd.stkPctCU = scales::percent(pd.stkCU/length(a.CoreData$TaggedPitchType))
pd.ballCU = length(which(p.CU$PitchCall == 'BallCalled'))
pd.bipCU = length(which(p.CU$PitchCall == 'InPlay'))
pd.smCU = length(which(p.CU$PitchCall == 'StrikeSwinging'))
pd.smPctCU = scales::percent(pd.smCU/length(a.CoreData$TaggedPitchType))
# PGR Tables ----
# * Fastball ----
T1 = matrix(
  c(
    pd.nFB,
    pd.pFB,
    pd.maxFB,
    pd.avgFB,
    pd.spnFB,
    pd.tiFB,
    pd.hbFB,
    pd.ivbFB,
    pd.stkFB,
    pd.stkPctFB,
    pd.ballFB,
    pd.bipFB,
    pd.smFB,
    pd.smPctFB
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T1) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T1) <- c('Fastball')
# * Change ----
T2 = matrix(
  c(
    pd.nCH,
    pd.pCH,
    pd.maxCH,
    pd.avgCH,
    pd.spnCH,
    pd.tiCH,
    pd.hbCH,
    pd.ivbCH,
    pd.stkCH,
    pd.stkPctCH,
    pd.ballCH,
    pd.bipCH,
    pd.smCH,
    pd.smPctCH
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T2) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T2) <- c('Change') 
# * Slider ----
T3 = matrix(
  c(
    pd.nSL,
    pd.pSL,
    pd.maxSL,
    pd.avgSL,
    pd.spnSL,
    pd.tiSL,
    pd.hbSL,
    pd.ivbSL,
    pd.stkSL,
    pd.stkPctSL,
    pd.ballSL,
    pd.bipSL,
    pd.smSL,
    pd.smPctSL
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T3) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T3) <- c('Slider')
# * Splitter ----
T4 = matrix(
  c(
    pd.nSP,
    pd.pSP,
    pd.maxSP,
    pd.avgSP,
    pd.spnSP,
    pd.tiSP,
    pd.hbSP,
    pd.ivbSP,
    pd.stkSP,
    pd.stkPctSP,
    pd.ballSP,
    pd.bipSP,
    pd.smSP,
    pd.smPctSP
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T4) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T4) <- c('Splitter')
# * Curveball ----
T5 = matrix(
  c(
    pd.nCB,
    pd.pCB,
    pd.maxCB,
    pd.avgCB,
    pd.spnCB,
    pd.tiCB,
    pd.hbCB,
    pd.ivbCB,
    pd.stkCB,
    pd.stkPctCB,
    pd.ballCB,
    pd.bipCB,
    pd.smCB,
    pd.smPctCB
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T5) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T5) <- c('Curveball')
# * Sinker ----
T6 = matrix(
  c(
    pd.nSI,
    pd.pSI,
    pd.maxSI,
    pd.avgSI,
    pd.spnSI,
    pd.tiSI,
    pd.hbSI,
    pd.ivbSI,
    pd.stkSI,
    pd.stkPctSI,
    pd.ballSI,
    pd.bipSI,
    pd.smSI,
    pd.smPctSI
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T6) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T6) <- c('Sinker')
# * Cutter ----
T7 = matrix(
  c(
    pd.nCU,
    pd.pCU,
    pd.maxCU,
    pd.avgCU,
    pd.spnCU,
    pd.tiCU,
    pd.hbCU,
    pd.ivbCU,
    pd.stkCU,
    pd.stkPctCU,
    pd.ballCU,
    pd.bipCU,
    pd.smCU,
    pd.smPctCU
  ),
  ncol = 14,
  nrow = 1,
  byrow = TRUE
)
colnames(T7) <-
  c(
    'Num',
    'Use',
    'Max',
    'Avg',
    'Spin',
    'Tilt',
    'HB',
    'IVB',
    'StkCall',
    'StkPct',
    'BallCall',
    'BIP',
    'SM',
    'SMPct'
  )
rownames(T7) <- c('Cutter')
# BInd Info Tables --------------------------------------------------------
# bind matricies into one larger matrix
pd.Final = rbind(T1,T2,T3,T4,T5,T6,T7)
# Supplemental Datasets ---------------------------------------------------
# to tell it to draw strike zone for Spread Summary
g.x <- c(-.75,.75,.75,-.75,-.75)
g.z <- c(1.65,1.65,3.65,3.65,1.65)
#store strikezone in a dataframe for spread summary
g.strike.zone <- data.frame(g.x,g.z)
#Axis limits for Movement graph
ga.x <- c(0,0)
ga.z <- c(0,0)
#horizontal and vertical axis dataframe for movement graph
g.movement.axis <-data.frame(ga.x,ga.z)
# BIP Summary -------------------------------------------------------------
#Filter only for balls put in play
b.BIP = as.data.frame(filter(a.CoreData, PitchCall == 'InPlay'))
#create BIP table
b.BIPtbl = select(b.BIP, 'PitchNo', 'TaggedPitchType','TaggedHitType',
                  'PlayResult','ExitSpeed','Angle','Distance')
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
  xlim(-3,3) +
  ylim(.5,4.5) +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  labs(col='Pitch') +
  geom_text(data = b.BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())
# Spread Summary -------------------------------------------------------------
#data frames for respective batter sides
c.RHB = as.data.frame(filter(a.CoreData, BatterSide == 'Right' ))
c.LHB = as.data.frame(filter(a.CoreData, BatterSide == 'Left'))
#plot for vs RHB
g.RHBPlot = ggplot() +
  geom_point(data = c.RHB, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path( data = g.strike.zone, aes(x=g.x, y=g.z), size = 1.5, lineend = 'round',linejoin = 'round') + coord_equal() +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('vs RHB', subtitle = a.CoreData$Pitcher) +
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
#plot for vs LHB
g.LHBPlot = ggplot() +
  geom_point(data = c.LHB, aes(x=PlateLocSide,y=PlateLocHeight,color=TaggedPitchType), size= 6 ) +
  geom_path( data = g.strike.zone, aes(x=g.x, y=g.z), size = 1.5, lineend = 'round',linejoin = 'round') + coord_equal() +
  geom_segment( aes(x =  .25, y = 1.65, xend =  .25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.25, y = 1.65, xend = -.25, yend = 3.65),linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 3,    xend =  .75, yend = 3),   linetype=2, color='dark grey', size = 1) +
  geom_segment( aes(x = -.75, y = 2.3,  xend =  .75, yend = 2.3), linetype=2, color='dark grey', size = 1) +
  theme(legend.position = c(.9,0.1)) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle('vs LHB', subtitle = a.CoreData$Pitcher) +
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
# Release Summary -----------------------------------------------------------
g.Rel = ggplot() + geom_point(data = a.CoreData, aes(x=RelSide,y=RelHeight,color=TaggedPitchType), size = 4) + geom_vline(xintercept=0, size=1.5) + geom_hline(yintercept=4, size=1.5)+
  ggtitle('Release Points', subtitle = a.CoreData$Pitcher) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Release Side') +
  ylab('Release Height') +
  labs(col='Pitch') +
  scale_x_continuous(limits = c(-6,6)) +
  scale_y_continuous(limits = c(.25, 8)) +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  theme(panel.background = element_blank()) +
  theme(legend.position = c(.14,.88), legend.background = element_blank(),
        legend.key = element_blank()) +
  theme(plot.margin = margin(.3,.3,.3,.3,'cm')) 
# Pitch Movement Summary --------------------------------------------------
g.PM = ggplot() +
  geom_point(data = a.CoreData,aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType),size = 3) +
  geom_path(data = g.movement.axis, aes(x = ga.x, y = ga.z)) + coord_equal() +
  xlab("Horz Break") +
  ylab("IVB") +
  xlim(-28, 28) +
  ylim(-28, 28) +
  ggtitle('Pitch Movement', subtitle = a.CoreData$Pitcher) +
  scale_color_manual(values = c('blue', 'red', 'orange','black','yellow')) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_vline(xintercept = 0, size = 1.5) +
  theme(legend.position = c(.9, 0.1)) +
  theme(
    legend.position = c(.14, .88),
    legend.background = element_blank(),
    legend.key = element_blank()
  ) +
  labs(col = 'Pitch') +
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 

P3 = select(a.CoreData, 'PitchNo', 'Balls', 'Strikes',
            'TaggedPitchType', 'RelSpeed','SpinRate','InducedVertBreak',
            'HorzBreak', 'Extension','PitchCall','ExitSpeed')
# Get all information -----------------------------------------------------
g.BIP
ggsave('BIP.png', path = '/Users/garrettkemp/Documents/R-Files', 
       dpi = 500,width = 5, height = 4, units = c('in') ,limitsize = TRUE, bg = 'transparent')
ggarrange(g.RHBPlot, g.LHBPlot, g.Rel, g.PM)
ggsave('Pitches.png', path = '/Users/garrettkemp/Documents/R-Files', 
       dpi = 500,width = 10, height = 9, units = c('in') ,limitsize = TRUE, bg = 'transparent')

view(pd.Final)

view(b.BIPtbl)

view(P3)

# ggsave ------------------------------------------------------------------
#ggsave; assign a plot to a variable, put variable name as file name (filename.png)
# You have to do ggsave after creating each plot; ENTER command for plot then ggsave

#File names and respective label
  #BIP graph = BIP_PitcherName
  #RHB LHB graphs = RHB/LBH_PitcherName
  #Release Point graph = RP_PitcherName
  #Pitch Movement Graph = PM_PitcherName

unique(data$Pitcher)

MER = filter(data, PitcherTeam == "MER_BEA")

unique(MER$Pitcher)

for (i in unique(MER$Pitcher)) {
  rmarkdown::render("/Users/garrettkemp/Documents/R-Files/R-Code/AutoReports/2023TESTPGR.Rmd",
                    output_dir = "/Users/garrettkemp/Documents/Reports/Auto Reports",
                    params = list(player = i),
                    output_file=paste0("2_26_23_",i,"_vs_URI.pdf"), clean = TRUE)
}

# for the auto Report to work make the params player = "" (empty quotes)





ls(a.data$Pitcher)

subset

MyData = read.csv("/Users/garrettkemp/Documents/R-Files/Shiny APP/test.csv")

filter(MyData, BatterSide %in% c("Left","Right"))



paste("this is red",)