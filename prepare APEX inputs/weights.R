library(tidyverse)

## Prepare data
# Import cattle weights for 2014-2022
weights <- read.csv("D:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle_14-22_Nicole_Sep2023.csv")

# Add a column that removed the shrunk weight
weights$unshrunk_OnWeightToUse <- weights$OnWeightToUse / 0.93

## Summarize data
weights_APEXinput <- weights %>%
  group_by(Year, OnDate, Pasture) %>%
  summarize(input_wt_lbs = mean(unshrunk_OnWeightToUse)) %>%
  mutate(input_wt_kg = round((input_wt_lbs / 2.205), 2))

## Export data
write.csv(weights_APEXinput,"D:/APEX data and scripts/APEX inputs/cattle weights_APEXinputs_2014-2022.csv")
