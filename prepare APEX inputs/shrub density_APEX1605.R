library(tidyverse)
library(rstatix)

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
  identify_outliers(density) %>%
  dplyr::filter(is.extreme == TRUE)

# remove outliers
shrub.density_NOoutliers <- shrub.density %>%
  anti_join(shrub.density_outliers)

##### SUMMARIZING TO PLOT (92 SUBAREAS) ########################################
# import data
PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

# summarizing data to plot
shrub.summ_plot <- shrub.density %>%
  # grouping func. groups into SSHB or ATCA
  mutate(CPNM = ifelse(CPNM %in% c("ARFR", "EREF", "ERNA", "GUSA", "KRLA", "YUGL"),
                       "SSHB", "ATCA")) %>%
  group_by(YEAR, Pasture, Plot, Ecosite, CPNM) %>%
  summarize(density = round(mean(density), 2)) %>%
  filter(YEAR %in% c(2013:2018))

# APEX plant codes
apex.code <- c(340, 351)
apex.FG <- c("SSHB", "ATCA")

APEX.codes <- data.frame(apex.code, apex.FG)

# adding pastureID and APEX plant code
opc.input_shrub <- merge(shrub.summ_plot, PastureID,
                              by = c("Pasture", "Ecosite", "Plot"),
                              all.x = TRUE) %>%
  merge(APEX.codes, by.x = "CPNM", by.y = "apex.FG", all.x = TRUE) %>%
  group_by(ID, Treatment, apex.code) %>%
  summarize(apex.input = round(mean(density), 2))
  

# exporting data
write.csv(opc.input_shrub, "D:/APEX data and scripts/Data/mean.shrub.density_92subareas_TEST.csv")
