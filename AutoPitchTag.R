# Auto Pitch Tagging

data = read.csv("2024 Full Season Data - Sheet1 copy.csv")

P = 
data %>% 
  mutate(tag = case_when(RelSpeed > 85 & SpinRate > 1800 & InducedVertBreak > 0  ~ "Fastball",
                         RelSpeed < 85 & SpinRate < 2000 & InducedVertBreak > 0 ~ "ChangeUp",
                         RelSpeed < 90 & SpinRate > 1800 & InducedVertBreak < 10 & abs(HorzBreak) > 0 ~ "Slider"), .after = TaggedPitchType)

filter(P, PitcherTeam == "MER_BEA") %>% 
select(Pitcher, TaggedPitchType, tag, RelSpeed, SpinRate, InducedVertBreak, HorzBreak) %>% view()


