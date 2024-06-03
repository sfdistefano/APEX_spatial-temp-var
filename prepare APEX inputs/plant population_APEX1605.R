## Code written by Dr. Sean Di Stefano in 4/2024 for the Central Plains Experimental Range, Nunn, CO
library(tidyverse)
library(rstatix)

setwd("D:/APEX data and scripts/Data")

###### SUMMARIZING DATA BY PLOT (92 subareas) ##################################
PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

##### BASAL AREA ###############################################################
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
basal.area_NOoutliers <- basal.area #%>%
  # anti_join(basal.area_outliers)

# adding APEX code
apex.code <- c(300,300,310,321,330,340,351) # functional group code in APEX from CROP.DAT
apex.FG <- c("WSPG", "BOBU", "CSPG", "CSAG", "FORB", "SSHB", "ATCA") # functional group name

APEX.codes <- data.frame(apex.code, apex.FG)

basal.area_plot <- merge(basal.area_NOoutliers, APEX.codes,
                         by.x = "FunctionalGroupCode",
                         by.y = "apex.FG",
                         all.x = TRUE) %>%
  filter(Year %in% c(2014:2018) & !(is.na(apex.code)))

# adding info
opc.input_basal <- merge(basal.area_plot, PastureID,
                         by = c("Pasture", "Treatment","Ecosite", 
                                "Plot", "Block"),
                         all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(meanBasalcoverFG), 2)) 

##### SHRUB DENSITY ############################################################
shrub.density <- read.csv("D:/APEX data and scripts/Data/CPER Plant Pop/CARM_shrDensities2013_2023_cln_attr2023-12-28.CSV") %>%
  dplyr::rename(Pasture = PASTURE, Plot = PLOT) %>%
  dplyr::filter(!(Plot %in% c(5:6) & Pasture %in% c("18S", "19N"))) %>% # removing burn plots
  pivot_longer(cols = c(ARFR:KRLA), # shrub functional groups
               names_to = "CPNM", values_to = "density") 

# renaming NH to 10S
shrub.density$Pasture <- gsub(pattern = "NH", replacement = "10S",
                              x = shrub.density$Pasture)

# identify outliers for each functional group
shrub.density_outliers <- shrub.density %>%
  group_by(Ecosite, CPNM) %>%
  identify_outliers(density) #%>%
  # dplyr::filter(is.extreme == TRUE)

# remove outliers
shrub.density_NOoutliers <- shrub.density %>%
  anti_join(shrub.density_outliers)

# summarizing data to plot
shrub.summ_plot <- shrub.density %>%
  # grouping func. groups into SSHB or ATCA
  mutate(CPNM = ifelse(CPNM %in% c("ARFR", "EREF", "ERNA", "GUSA", "KRLA", "YUGL"),
                       "SSHB", "ATCA")) %>%
  group_by(YEAR, Pasture, Plot, Ecosite, CPNM) %>%
  summarize(density = round(mean(density), 2)) %>%
  filter(YEAR %in% c(2014:2018))

# adding pastureID and APEX plant code
opc.input_shrub <- merge(shrub.summ_plot, PastureID,
                         by = c("Pasture", "Ecosite", "Plot"),
                         all.x = TRUE) %>%
  merge(APEX.codes, by.x = "CPNM", by.y = "apex.FG", all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(density), 2))

##### COMBINE & EXPORT DATA ####################################################
# combine both data types
plant.pop <- rbind(opc.input_basal,opc.input_shrub)

# write.csv(plant.pop, "D:/APEX data and scripts/APEX inputs/input_OPC_subareas92.csv")

###### SUMMARIZING DATA BY PASTURE (20 subareas) ###############################
PastureID_sa20 <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
  mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))

##### BASAL AREA ###############################################################
opc.input_basal_sa20 <- merge(basal.area_plot, PastureID_sa20,
                              by = c("Pasture", "Treatment", "Block"),
                              all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(meanBasalcoverFG), 2),
            fg_min = round(min(meanBasalcoverFG),2),
            fg_max = round(max(meanBasalcoverFG),2)) 

##### SHRUB DENSITY ############################################################
opc.input_shrub_sa20 <- merge(shrub.summ_plot, PastureID_sa20,
                              by = "Pasture",
                              all.x = TRUE) %>%
  merge(APEX.codes, by.x = "CPNM", by.y = "apex.FG", all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(density), 2),
            fg_min = round(min(density),2),
            fg_max = round(max(density),2))

##### COMBINE & EXPORT DATA ####################################################
# combine both data types
plant.pop_sa20 <- rbind(opc.input_basal_sa20,opc.input_shrub_sa20)

write.csv(plant.pop_sa20, "D:/APEX data and scripts/APEX inputs/input_OPC_subareas20.csv")
