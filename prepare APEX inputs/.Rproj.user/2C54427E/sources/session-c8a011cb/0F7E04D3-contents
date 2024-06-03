library(tidyverse)

setwd("D:/APEX data and scripts/")

# Import pasture reference information
pastureID <- read.csv("Data/PastureID_ecosite_20subareas.csv") %>%
  # Converting pasture name to match CARM name
  mutate(Pasture = gsub("10S", "NH", Pasture)) %>%
  # Removing unnecessary column
  select(-Block)

# Import CARM rotation and stocking density data
cattle.data <- read.csv("Data/CPER Cattle/CARM_ActualGrazingInfov4_2013-2023.csv") %>%
  # Removing unnecessary columns
  select(PastureCode, Year, Rest, NumSteers, RotationOrder, 
         DateInPasture, DateOutPasture, split_herd)

# Add pasture information & remove pastures that aren't in CARM
cattle.data_merge <- cattle.data %>% 
  merge(pastureID, by.x = "PastureCode", by.y = "Pasture") %>%
  # Calculate ha/animal (input for APEX OPC)
  mutate(ha_anim = round((PastureSize_ha / NumSteers), 2)) 


## Export data
write.csv(cattle.data_merge,"APEX inputs/stocking rate_APEXinputs_2012-2023.csv")

  




  
