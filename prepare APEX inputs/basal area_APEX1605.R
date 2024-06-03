library(tidyverse)
library(rstatix)

setwd("D:/APEX data and scripts/Data")

##### SUMMARIZING DATA BY PLOT (92 subareas) ###################################
PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

basal.area <- read.csv("D:/APEX data and scripts/Data/CPER Plant Pop/BasalCover_FuncGrpByTransect_CARM_wide2013-2023.csv") %>%
  pivot_longer(cols = BARE:WSPG,
               names_to = "FunctionalGroupCode",
               values_to = "meanBasalcoverFG") %>%
  mutate(Treatment = ifelse(Treatment == "AGM", "CARM", "TRM"))

# renaming NH to 10S
basal.area$Pasture <- gsub(pattern = "NH", replacement = "10S",
                                     x = basal.area$Pasture)

# identify outliers for each functional group
basal.area_outliers <- basal.area %>%
  group_by(Ecosite, FunctionalGroupCode) %>%
  identify_outliers(meanBasalcoverFG) %>%
  dplyr::filter(is.extreme == TRUE)

# remove outliers
basal.area_NOoutliers <- basal.area %>%
  anti_join(basal.area_outliers)

# adding APEX code
apex.code <- c(300,300,310,321,330,340,351) # functional group code in APEX from CROP.DAT
apex.FG <- c("WSPG", "BOBU", "CSPG", "CSAG", "FORB", "SSHB", "ATCA") # functional group name

APEX.codes <- data.frame(apex.code, apex.FG)

basal.area_plot <- merge(basal.area_NOoutliers, APEX.codes,
                              by.x = "FunctionalGroupCode",
                              by.y = "apex.FG",
                              all.x = TRUE) %>%
  filter(Year %in% c(2013:2018) & !(is.na(apex.code)))

# adding info
opc.input_basal <- merge(basal.area_plot, PastureID,
                            by = c("Pasture", "Treatment","Ecosite", 
                                   "Plot", "Block"),
                            all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(meanBasalcoverFG), 2)) 

# exporting data
write.csv(opc.input_basal, "mean.basal.area_92subareas_TEST.csv")

##### SUMMARIZING DATA BY PASTURE #################################################
# import data
# PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")
# 
# basal.area <- read.csv("D:/APEX data and scripts/Data/BasalCover_FuncGrpByPlot_CARM_wide.csv") %>%
#   pivot_longer(cols = BARE:WSPG,
#                names_to = "FunctionalGroupCode",
#                values_to = "meanBasalcoverFG")
# 
# ## summarizing basal area cover by year, pasture, and functional group
# basal.area.summ_plot <- basal.area %>%
#   group_by(Year, Pasture, Plot, # summarizing by plot
#            Treatment, Ecosite, FunctionalGroupCode) %>%
#   summarize(mean.basalarea = mean(meanBasalcoverFG)) %>%
#   filter(!(Plot == 5 & Pasture == "18S")) %>%
#   filter(!(Plot == 6 & Pasture == "18S")) %>%
# filter(!(Plot == 5 & Pasture == "19N")) %>%
#   filter(!(Plot == 6 & Pasture == "19N"))
# 
# ## adding APEX code
# apex.code <- c(300,300,310,321,330,340,351) # functional group code in APEX from CROP.DAT
# apex.FG <- c("WSPG", "BOBU", "CSPG", "CSAG", "FORB", "SSHB", "ATCA") # functional group name
# 
# APEX.codes <- data.frame(apex.code, apex.FG)
# 
# basal.area.summ_plot <- merge(basal.area.summ_plot, APEX.codes,
#                               by.x = "FunctionalGroupCode",
#                               by.y = "apex.FG",
#                               all.x = TRUE) %>%
#   filter(Year %in% c(2013:2018))
# 
# ## adding pasture information
# 
# # renaming NH to 10S
# basal.area.summ_plot$Pasture <- gsub(pattern = "NH", replacement = "10S",
#                                      x = basal.area.summ_plot$Pasture)
# 
# # adding info
# basal.area_pasture <- merge(basal.area.summ_plot, pastureID,
#                               by = "Pasture",
#                               all.x = TRUE) %>%
#   filter(!(is.na(apex.code))) %>%
#   group_by(Year, PastureID, apex.code) %>%
#   summarize(apex.input = signif(mean(mean.basalarea), 4))
# 
# # exporting data
# # write.csv(basal.area_pasture, "mean.basal.area_pasture.csv")