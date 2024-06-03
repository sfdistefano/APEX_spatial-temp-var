library(tidyverse)
library(rstatix)

# reference information
PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

# renaming treatment types to match APEX outputs
PastureID$Treatment <- gsub(pattern = "TRM",
                            replacement = "TGM",
                            x = PastureID$Treatment)

PastureID$Treatment <- gsub(pattern = "CARM",
                            replacement = "AGM",
                            x = PastureID$Treatment)

##### BASAL AREA ###############################################################
basal.area <- read.csv("D:/APEX data and scripts/Data/CPER Plant Pop/BasalCover_FuncGrpByTransect_CARM_wide2013-2023.csv") %>%
  pivot_longer(cols = BARE:WSPG,
               names_to = "FunctionalGroupCode",
               values_to = "meanBasalcoverFG")

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
  filter(Year %in% c(2013:2023) & !(is.na(apex.code)))

# rename BOBU to WSPG (BOBU is not currently separated from WSPG in model)
basal.area_plot$FunctionalGroupCode <- gsub(pattern = "BOBU",
                                            replacement = "WSPG",
                                            x = basal.area_plot$FunctionalGroupCode)

# adding reference info
canopy.input_basal <- merge(basal.area_plot, PastureID,
                            by = c("Pasture", "Treatment","Ecosite", "Plot"),
                            all.x = TRUE) %>%
  group_by(Year, Treatment, Pasture, ID, apex.code) %>%
  summarize(apex.input = round(mean(meanBasalcoverFG), 2))

##### SHRUB DENSITY ############################################################
shrub.density <- read.csv("D:/APEX data and scripts/Data/CPER Plant Pop/CARM_shrDensities2013_2023_cln_attr2023-12-28.csv") %>%
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
  identify_outliers(density) %>%
  dplyr::filter(is.extreme == TRUE)

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
  filter(YEAR %in% c(2013:2023))

# APEX plant codes
apex.code <- c(340, 351)
apex.FG <- c("SSHB", "ATCA")

APEX.codes <- data.frame(apex.code, apex.FG)

# adding pastureID and APEX plant code
canopy.input_shrub <- merge(shrub.summ_plot, PastureID,
                            by = c("Pasture", "Ecosite", "Plot"),
                            all.x = TRUE) %>%
  merge(APEX.codes, by.x = "CPNM", by.y = "apex.FG", all.x = TRUE) %>%
  rename(Year = YEAR) %>%
  group_by(Year, Treatment, Pasture, ID, apex.code) %>%
  summarize(apex.input = round(mean(density), 2))

##### COMBINING OUTPUTS ########################################################
canopy.input <- rbind(canopy.input_basal, canopy.input_shrub) %>%
  mutate(Month = 1, Day = 1) %>% # adding columns needed in CANOPY.DAT
  relocate(c(Month, Day), .after = Year) %>% # ordering to match CANOPY.DAT
  select(-Pasture)

write.csv(canopy.input, "D:/APEX data and scripts/APEX inputs/input_CANOPY.DAT_subareas92_TEST.csv")
