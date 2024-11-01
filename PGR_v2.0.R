# Fall 2024 Updated Post Game Report

# NOTES NOTES NOTES NOTES NOTES NOTES ----
# Do a table group by count and combine balls and strikes into new colm then add 
# frequencies

# Clear golobal environment
rm(list =ls())

# Packages ----
library(pacman)
pacman::p_load(tidyverse, ggthemes, rio, scales, ggpubr, clipr)
library(googlesheets4)
#Half of these packages aren't needed but I don't have the time to go thru rn
#data = read.csv(file.choose())

unique(data$Pitcher)

#Data Here ----
data = 
  read_sheet("https://docs.google.com/spreadsheets/d/1RoqNAGSxpRtpZ1Oz6J0QPrn-eJDkALeApc8dR7iaq2A/edit?gid=0#gid=0")

# Pitcher ----
P = filter(data, Pitcher == "Olson, Bryant")
subset = P[, c("RelSpeed")]
P = P[complete.cases(subset),]
# PC stands for Pitchers counts meaning two strikes excluding 3-2 counts
table = P %>% 
  group_by('Pitch' = TaggedPitchType) %>%
  summarise('Total' = n(),
            'Usage' = percent(n()/length(P$TaggedPitchType)),
            'Max' = floor(max(RelSpeed, na.rm = TRUE)),
            'Avg' = floor(mean(RelSpeed, na.rm = TRUE)),
            'Spin' = floor(mean(SpinRate, na.rm = TRUE)),
            'Tilt' = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
              as.numeric() %>% mean() %>%
              as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
              format(format = "%k:%M", tz = 'UTC'),
            'HB' = round(mean(HorzBreak, na.rm = TRUE), 2),
            'IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 2),
            #'Strikes' = length(which(PitchCall == "StrikeCalled")),
            'Strike %' = percent(length(which(PitchCall == "StrikeCalled"))/n()),
            'Whiff %' = percent(length(which(PitchCall == "StrikeSwinging"))/n()),
            #'LHB' = length(which(BatterSide == "Left")),
            'LHB %' = percent(length(which(BatterSide == "Left"))/n()),
            #'RHB' = length(which(BatterSide == "Right")),
            'RHB %' = percent(length(which(BatterSide == "Right"))/n()),
            'FP' = length(which(Balls == 0 & Strikes == 0)),
            'PC' = length(which(Strikes == 2 & Balls != 3))
            )
table = table %>% mutate(Pitch = recode(table$Pitch, 
                                'Fastball' = 'FB',
                                'TwoSeamFastBall' = '2SFB',
                                'Sinker' = 'SI',
                                'Cutter' = 'CU',
                                'Splitter' = 'SP',
                                'ChangeUp' = 'CH',
                                'Curveball' = 'CB',
                                'Slider' = 'SL'))
# BIP Summary -------------------------------------------------------------
#Filter only for balls put in play
BIP = as.data.frame(filter(P, PitchCall == 'InPlay'))
#create BIP table
BIPtbl = select(BIP, 'PitchNo', 'TaggedPitchType','TaggedHitType',
                  'PlayResult','ExitSpeed','Angle','Distance')
cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A')
bks = c('Fastball','TwoSeamFastBall','Sinker','Cutter','Splitter','ChangeUp','Curveball','Slider')
lbs = c('FB','2SFB','SI','CU','SP','CH','CB','SL')
# BIP Plot ----
g.BIP = ggplot(data = BIP, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType),) +
  xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Balls In Play") +
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
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  geom_text(data = BIP, aes(label=PitchNo, x=PlateLocSide,y=PlateLocHeight), size=3, color='white') +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))
# Spread Summary -------------------------------------------------------------
RHB = as.data.frame(filter(P, BatterSide == 'Right' ))
LHB = as.data.frame(filter(P, BatterSide == 'Left'))
# RHB Plot ----
g.RHB = ggplot(data = RHB, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType),) +
  xlim(-3,3) + ylim(0,5) + 
  labs(color = "", title = "vs RHB") +
  labs(color = "") +
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
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  #annotate('text', x = 0, y = 5, label = "vs RHB", size = 5) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank())
# LHB Plot ----
g.LHB = ggplot(data = LHB, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType),) +
  xlim(-3,3) + ylim(0,5) + 
  labs(color = "", title = "vs LHB") +
  labs(color = "") +
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
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  #annotate('text', x = 0, y = 5, label = "vs LHB", size = 5) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank())
# Movement Plot (Change to g.PM) ----
g.PM = ggplot(data = P, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())
# Release Point Plot ----
g.RP = ggplot(data = P, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
  labs(x = "Release Side (ft)", y = "Release Height (ft)", color = " ", title = "Release Points") + 
  xlim(-5, 5) + ylim(-7, 7) +
  geom_segment(aes(x = 0, y = -7, xend = 0, yend = 7), size = 1, color = "grey55") + 
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 3, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_text(size = 14)) +
  theme(legend.background = element_blank(), legend.key = element_blank())
# Extension Testing ----
g.ESV = ggplot(data = P, aes(x = Extension*12, y = RelHeight*12, color = TaggedPitchType)) +
  labs(x = "Extension (in)", y = "Release Height (in)", color = " ", title = "Extension Side View") +
  geom_point(size = 2) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  coord_cartesian(xlim = c(0,95), ylim = c(0,80)) +
  geom_segment(aes(x = 0, y = 0, xend = 48, yend = 0), size = 2, color = 'black') +
  geom_curve(aes(x = 48, y = 0, xend = 85, yend = -4), size = 2, curvature = -.05, color = 'black') +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 8), axis.title = element_text(size = 14)) +
  theme(legend.background = element_blank(), legend.key = element_blank())
#negative release side is because of graph structure and release point being Pitcher POV
# ie LHP releases negative in Pitcher POV but in extension graph that is positive side/up
# g.ETV
g.ETV = ggplot(data = P, aes(x = Extension*12, y = RelSide*-12, color = TaggedPitchType)) +
  labs(x = "Extension (in)", y = "Release Side (in)", color = " ", title = "Extension Top View") +
  geom_point(size = 2) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs, drop = FALSE) +
  coord_cartesian(xlim = c(0,96), ylim = c(-48,48)) +
  geom_segment(aes(x = -6, y = 10, xend = 0, yend = 10), color = 'black', size = 1) +
  geom_segment(aes(x = -6, y = -10, xend = 0, yend = -10), color = 'black', size = 1) +
  geom_segment(aes(x = 0, y = -10, xend = 0, yend = 10), color = 'black', size = 1) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14)) +
  theme(legend.background = element_blank(), legend.key = element_blank())
#End ----


Plots = ggarrange(g.LHB, g.RHB, g.RP, g.PM, g.ESV, g.ETV)
ggsave('Plots.png', plot = Plots, path = '/Users/garrettkemp/Desktop', 
       dpi = 500,width = 12, height = 8, units = c('in') ,limitsize = TRUE, bg = 'transparent')
ggsave('BIP.png', plot = g.BIP, path = '/Users/garrettkemp/Desktop', 
       dpi = 500,width = 5, height = 4, units = c('in') ,limitsize = TRUE, bg = 'transparent')

write_clip(table, col.names = FALSE)

write_clip(BIPtbl, col.names = FALSE)

pl



#Dont worry about this
for (i in unique(MER$Pitcher)) {
  rmarkdown::render("/Users/garrettkemp/Documents/R-Files/R-Code/MarkdownFiles/2023TESTPGR.Rmd",
                    output_dir = "/Users/garrettkemp/Documents/Reports/Auto Reports",
                    params = list(player = i),
                    output_file=paste0("2_26_23_",i,"_vs_URI.pdf"), clean = TRUE)
}

# for the auto Report to work make the params player = "" (empty quotes)

data = read.csv(file.choose())

unique(data$Date)

dataA = filter(data, Date == "2023-10-06")

P = filter(data, Pitcher == "Cosper, Cade")

P %>%
  filter(TaggedPitchType == "Fastball") %>%
  ggplot(aes(x = Time, y = RelSpeed)) +
  geom_line(group = 1) +
  geom_point()


# My heatmap
ggplot(P, aes(x = PlateLocSide, y = PlateLocHeight)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
  scale_fill_gradientn(colours = c("blue", "white", "red")) +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
  ylim(0, 5) + xlim(-3, 3) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(fill = FALSE)


table(P$TaggedPitchType)%>% as.data.frame()%>%
  ggplot(aes(x = "", y = Freq, fill = Var1)) +
           geom_bar(stat = 'identity', width = 1) +
           coord_polar('y', start = 0) +
  theme_void() +
  guides(fill=guide_legend(title="Pitches"))


# Create Data
dataA <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Basic piechart
ggplot(dataA, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

W = filter(P, KorBB == "Walk")%>%select(Pitcher, PitchofPA, TaggedPitchType)

shift(W, n=1, fill = NA, type = "lag")

