# Packages ----
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,tidyverse,sportyR,scales,
               ggpubr,knitr)

rm(list=ls())

data = read.csv(file.choose())

data[5,]

select(data, 1:5)


pit = data[data$Pitcher == 'Cosper, Colton',]

ggplot(data = pit, aes(x = HorzBreak/12 , y = InducedVertBreak/12 , color = TaggedPitchType)) +
  labs(x = "Horizontal Movement (ft.)", y = "Vertical Movement (ft.)", color = " ", title = "Pitch Movement") + 
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  geom_segment(aes(x = 0, y = -2.5, xend = 0, yend = 2.5), size = 1, color = "grey55") + 
  geom_segment(aes(x = -2.5, y = 0, xend = 2.5, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))

data$InducedVertBreak/12


data

mean(data$RelSpeed%>%na.omit())

mean(a.Pitcher$RelSpeed%>%na.omit())


n(a.Pitcher$TaggedPitchType)

length(a.Pitcher$TaggedPitchType[a.Pitcher$Pitcher == 'Cosper, Colton'])

length(data$TaggedPitchType[data$Pitcher == 'Cosper, Colton'])

length(data$TaggedPitchType)

mean(data$Tilt)

c(a.CoreData$Tilt[a.CoreData$TaggedPitchType == 'Fastball'])

timestr

MER = read.csv("/Users/garrettkemp/Documents/R-Files/Shiny APP/test.csv")

P = filter(MER, Pitcher == 'Harlow, Josh')

pp = filter(P, TaggedPitchType == 'Fastball')

round(seconds_to_period(mean(period_to_seconds(hm(c(pp$Tilt[a.CoreData$TaggedPitchType == 'Fastball']))))))%>%
  gsub('[HMS]','', .)%>%as.data.frame()%>%separate('.', c('x','y'))%>%unite('z',x:y,sep = ':')


gsub('[HMS]','', timestr)%>%as.data.frame()%>%separate('.', c('x','y'))%>%unite('z',x:y,sep = ':')

expand_grid(x = hour, y = minute)

round(99.2)

ceiling(99.2)

floor(99.2)

length(which(P$PitchCall == "StrikeCalled"))

# useful to make a quick table AUG 6 2023

table = P %>% 
  group_by('Pitch' = TaggedPitchType) %>%
  summarise('Total' = n(),
            'Usage' = percent(n()/length(P$TaggedPitchType)),
            'Max' = floor(max(RelSpeed, na.rm = TRUE)),
            'Avg' = floor(mean(RelSpeed, na.rm = TRUE)),
            'Spin' = floor(mean(SpinRate, na.rm = TRUE)),
            'Tilt' = round(seconds_to_period(mean(period_to_seconds(hm(c(Tilt%>%na.omit()))))))%>%
              gsub('[HMS]','', .)%>%as.data.frame()%>%separate('.', c('x','y'))%>%unite('z',x:y,sep = ':'),
            'HB' = round(mean(HorzBreak, na.rm = TRUE), 2),
            'IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 2),
            #'Strikes' = length(which(PitchCall == "StrikeCalled")),
            'Strike %' = percent(length(which(PitchCall == "StrikeCalled"))/n())
  )



