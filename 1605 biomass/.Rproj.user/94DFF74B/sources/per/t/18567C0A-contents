library(tidyverse)

# Load data and perform initial transformations
observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", header = TRUE) %>%
  select(2:13, 17) %>%
  mutate(CSAG = AG, WSPG = (BOBU + WSPG) / 2, CSPG = C3PG) %>%
  dplyr::rename(Year = YearSampled) %>%
  select(1:5, 9:15) %>%
  filter(Year %in% 2014:2018) %>%
  pivot_longer(cols = c(FORB:SS, CSAG:CSPG), names_to = "APEXcodeFG", values_to = "v1") %>%
  group_by(Year, Pasture, Plot, Transect, Treatment, APEXcodeFG) %>%
  summarize(v1 = mean(v1), .groups = 'drop')

# Clean observed data by plot
observed_data_plot <- observed_data %>%
  group_by(Year, Pasture, Plot, Treatment, APEXcodeFG) %>%
  summarize(MeankgPerHa = mean(v1), .groups = 'drop') %>%
  filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") & Treatment == "TGM") %>%
  mutate(Type = "Observed",
         Date = ymd(paste(Year, 8, 6, sep = "-")),
         WS = NA,
         APEXcodeFG = as.character(APEXcodeFG)) %>%
  select(-Year)

# Function to change plant codes
change_plant_codes <- function(df) {
  df %>%
    mutate(APEXcodeFG = recode(APEXcodeFG, "SS" = "SSHB", "CSAG" = "VUOC", "FORB" = "FRB3"),
           Treatment = recode(Treatment, "TGM" = "TRM", "AGM" = "CARM"),
           Pasture = recode(Pasture, "NH" = "10S"))
}

# Apply plant code changes to observed data by plot
observed_data_plot <- change_plant_codes(observed_data_plot)

# Summarize observational data by pasture
observed_data_pasture <- observed_data %>%
  group_by(Year, Treatment, Pasture, APEXcodeFG) %>%
  summarize(MeankgPerHa = mean(v1), .groups = 'drop') %>%
  filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") & Treatment %in% c("AGM", "TGM")) %>%
  mutate(Type = "Observed",
         Date = ymd(paste(Year, 8, 6, sep = "-")),
         WS = NA,
         APEXcodeFG = as.character(APEXcodeFG)) %>%
  select(-Year)

# Apply plant code changes to observed data by pasture
observed_data_pasture <- change_plant_codes(observed_data_pasture)

# Import simulated data for plot level
pathTRM <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"

simulated_biomass_TRM <- import_and_filter_data(pathTRM) %>%
  mutate(Treatment = "TRM")

# Import and clean reference information for plot level
PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

simulated_biomass_clean_TRM <- clean_biomass_data(simulated_biomass_TRM)

# Filter simulated data for August 6th and merge with observed data by plot
simulated_plot_aug6 <- simulated_biomass_clean_TRM %>%
  filter(month(Date) == 8, day(Date) == 6, Treatment == "TRM")

observed_simulated_plot_merge <- merge(simulated_plot_aug6, observed_data_plot,
                                       by = c("Pasture", "APEXcodeFG", "Date"))

# Import simulated data for pasture level
PastureID_pasture <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")
simulated_biomass_clean_TRM_pasture <- clean_biomass_data(simulated_biomass_TRM)

# Filter simulated data for August 6th and merge with observed data by pasture
simulated_pasture_aug6 <- simulated_biomass_clean_TRM_pasture %>%
  filter(month(Date) == 8, day(Date) == 6, Treatment == "TRM")

observed_simulated_pasture_merge <- merge(simulated_pasture_aug6, observed_data_pasture,
                                          by = c("Pasture", "APEXcodeFG", "Date"))

# Import second simulated data for plot level
pathTRM_dyn <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"
simulated_biomass_TRM_dyn <- import_and_filter_data(pathTRM_dyn) %>%
  mutate(Treatment = "TRM")

# Clean second simulated data for plot level
simulated_biomass_clean_TRM_dyn <- clean_biomass_data(simulated_biomass_TRM_dyn)

# Filter second simulated data for August 6th and merge with observed data by plot
simulated_plot_aug6_dyn <- simulated_biomass_clean_TRM_dyn %>%
  filter(month(Date) == 8, day(Date) == 6, Treatment == "TRM")

observed_simulated_plot_merge_dyn <- merge(simulated_plot_aug6_dyn, observed_data_plot,
                                           by = c("Pasture", "APEXcodeFG", "Date"))
