## Code written by Dr. Sean Di Stefano in 4/2024 for the Central Plains Experimental Range, Nunn, CO
library(tidyverse)

setwd("C:/APEX data and scripts/Data")

###### SUMMARIZING DATA BY PLOT (92 subareas) ##################################
PastureID_sa92 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

##### BASAL AREA ###############################################################
basal.area <- read.csv("C:/APEX data and scripts/Data/CPER Plant Pop/BasalCover_FuncGrpByTransect_CARM_wide2013-2023.csv") %>%
  pivot_longer(cols = BARE:WSPG,
               names_to = "FunctionalGroupCode",
               values_to = "meanBasalcoverFG") %>%
  mutate(Treatment = ifelse(Treatment == "AGM", "CARM", "TRM")) # renaming grazing treatment

# renaming NH to 10S
basal.area$Pasture <- gsub(pattern = "NH", replacement = "10S",
                           x = basal.area$Pasture)

# adding APEX code
apex.code <- c(300,301,310,321,330,340,351) # functional group code in APEX from CROP.DAT
apex.FG <- c("WSPG", "BOBU", "CSPG", "CSAG", "FORB", "SS", "ATCA") # functional group name

APEX.codes <- data.frame(apex.code, apex.FG)

basal.area_plot <- merge(basal.area, APEX.codes,
                         by.x = "FunctionalGroupCode",
                         by.y = "apex.FG",
                         all.x = TRUE) %>%
  filter(Year %in% c(2014:2018),
         !(is.na(apex.code)))

# adding info and summarizing data by Pasture * Plot * Plant Functional Group
opc.input_basal <- merge(basal.area_plot, PastureID_sa92,
                         by = c("Pasture", "Treatment","Ecosite", 
                                "Plot", "Block"),
                         all.x = TRUE) %>%
  group_by(Pasture, Plot, ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(meanBasalcoverFG), 2)) %>%
  # Add a new group with apex.code 300 to combine 300 and 301 (both warm-season perennial grasses [WSPG])
  mutate(apex.code = ifelse(apex.code %in% c(300, 301), 300, apex.code)) %>%
  group_by(Pasture, Plot, ID, Treatment, apex.code) %>%
  summarize(apex.input = sum(apex.input)) %>%
  filter(apex.code != 340) %>%
  ungroup()

##### SHRUB DENSITY ############################################################
shrub.density <- read.csv("C:/APEX data and scripts/Data/CPER Plant Pop/CARM_shrDensities2013_2023_cln_attr2023-12-28.CSV") %>%
  dplyr::rename(Pasture = PASTURE, Plot = PLOT) %>%
  dplyr::filter(!(Plot %in% c(5:6) & Pasture %in% c("18S", "19N"))) %>% # removing burned plots
  pivot_longer(cols = c(ARFR:KRLA), # shrub functional groups
               names_to = "CPNM", values_to = "density") 

# renaming NH to 10S (NH is the stakeholder name for 10S)
shrub.density$Pasture <- gsub(pattern = "NH", replacement = "10S",
                              x = shrub.density$Pasture)

## summarizing data to plot
shrub.summ_plot <- shrub.density %>%
  group_by(YEAR, Pasture, Plot, Ecosite, CPNM) %>%
  summarize(density = round(mean(density), 2)) %>%
  filter(YEAR %in% c(2014:2018))

# calculating average density for CPNM = "SS" by YEAR, Pasture, Ecosite, and Plot
summed_ss_density <- shrub.summ_plot %>%
  filter(CPNM %in% c("ARFR", "EREF", "ERNA", "GUSA", "KRLA", "YUGL")) %>%
  mutate(CPNM = "SS") %>% # creating a new category for summed SS
  group_by(YEAR, Pasture, Plot, Ecosite, CPNM) %>%
  summarize(density = round(sum(density), 2)) 

# separate ATCA rows for later combination
atca_density <- shrub.summ_plot %>%
  filter(CPNM == "ATCA")

# combining both plant types to shrub.summ_plot (SS [sub-shrub] or ATCA [four-wing saltbush])
shrub.summ_plot <- bind_rows(summed_ss_density, atca_density)

## adding pastureID and APEX plant code,summarizing data by Pasture * Plant Functional Group
opc.input_shrub <- merge(shrub.summ_plot, PastureID_sa92,
                         by = c("Pasture", "Ecosite", "Plot"),
                         all.x = TRUE) %>%
  merge(APEX.codes, by.x = "CPNM", by.y = "apex.FG", all.x = TRUE) %>%
  group_by(Pasture, Plot, ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(density), 2))

##### COMBINE & EXPORT DATA ####################################################
# combine both data types
plant.pop <- rbind(opc.input_basal,opc.input_shrub)

write.csv(plant.pop, "C:/APEX data and scripts/APEX inputs/input_OPC_subareas92_11022024.csv")

###### SUMMARIZING DATA BY PASTURE (20 subareas) ###############################
PastureID_sa20 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
  mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))

plant.pop_sa20 <- merge(plant.pop %>% dplyr::select(-ID), # removing plot ID to keep pasture-level ID from PastureID_sa20 dataframe
                        PastureID_sa20,
                        by = c("Pasture", "Treatment"),
                        all.x = TRUE) %>%
  group_by(Pasture, ID, Treatment, apex.code) %>% 
  summarize(apex.input_sa20 = round(mean(apex.input), 2),
            # calculating the range of biomass production within each pasture
            fg_min = round(min(apex.input),2), 
            fg_max = round(max(apex.input),2)) %>%
  rename(apex.input = apex.input_sa20)

write.csv(plant.pop_sa20, "C:/APEX data and scripts/APEX inputs/input_OPC_subareas20_10312024.csv")