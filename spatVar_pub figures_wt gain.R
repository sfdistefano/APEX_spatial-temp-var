library(tidyverse)
library(reshape2)
library(data.table)
library(ggsci)
library(hydroGOF)

##### ANNUAL GRAZING FILE (.AGZ) ###############################################

# Importing reference information for pastures
PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
PastureID_20sa <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

## Importing observed annual mean weight gain data
observed_weight_data <- read.csv("C:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2023_SD.csv") %>%
  mutate(gain = ADG / 2.2046) %>%  # Converting from lbs to kg
  merge(PastureID_20sa, by = 'Pasture', 
        relationship = "many-to-many", all.x = TRUE) %>%
  mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM"))

# Summarizing observed data for each treatment type
observed_weight_data_filtered <- observed_weight_data %>%
  select(Year, Treatment, gain) %>%
  filter(Year %in% c(2014:2018)) %>%
  group_by(Year, Treatment) %>%
  summarize(MeanWT = mean(gain, na.rm = TRUE), 
            SD = sd(gain), 
            Type = "Measured", .groups = 'drop')

### Function to read and process simulated data by grazing treatment
prepare_weight_data_agz <- function(file_path, years, id_filter, 
                                    sim_type, Pasture_data) {
  data.table::fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years, ID %in% id_filter) %>%
    left_join(Pasture_data, by = "ID") %>%
    group_by(YR, Treatment) %>%
    summarize(MeanWT = mean(WTGkg.hd.d, na.rm = TRUE)) %>%
    mutate(Type = sim_type, SD = 0) %>%
    rename(Year = YR)
}
## Importing simulated data using the reusable function
years <- c(2014:2018)

# Baseline simulation scenario
grazing_weight_data_noVar_yearly <- prepare_weight_data_agz(
  "C:/01-APEX1605_CO_baseline/WtGain_Simulation/APEX1605_CO_all20/CONUNN_TGM.AGZ",
  years, 1:20, Pasture_data = PastureID_20sa,
  "Simulated: no variability"
)

# Spatial variability simulation scenario
grazing_weight_data_spatVar_yearly <- prepare_weight_data_agz(
  "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.AGZ",
  years, 1:92, Pasture_data = PastureID,
  "Simulated: spatial variability"
)

# Spatial + temporal variability simulation scenario
# grazing_weight_data_spatTempVar_yearly <- prepare_weight_data_agz(
#   "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.AGZ",
#   years, 1:92, Pasture_data = PastureID,
# "Simulated: spatial + temporal variability"
# )

## Combining all grazing observed and simulated data
grazing_weight_data_yearly <- rbind(
  observed_weight_data_filtered, grazing_weight_data_noVar_yearly, 
  grazing_weight_data_spatVar_yearly#, grazing_weight_data_spatTempVar_yearly
)

# Reorder the levels of the Type column
grazing_weight_data_yearly$Type <- factor(grazing_weight_data_yearly$Type, 
                                          levels = c("Measured", 
                                                     "Simulated: no variability", 
                                                     "Simulated: spatial variability"#, 
                                                     #"Simulated: spatial + temporal variability"
                                          ))

# Create a bar plot with error bars
# Create combined ggplot for TRM and CARM
ggplot_grazing_yearly <- ggplot(grazing_weight_data_yearly, aes(x = factor(Year), y = MeanWT, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bars for mean weight
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars
  facet_wrap(~ Treatment) +  # Facet by Treatment (TRM and CARM)
  labs(x = "Year", 
       y = "Mean Daily Weight Gain (kg/head/day)", 
       fill = "Data Source") +  # Labels and legend title
  theme_bw() +   # Clean theme
  scale_fill_brewer(palette = "Set3") +
  theme(text = element_text(size = 15, family = 'serif'))

# Print the combined plot
print(ggplot_grazing_yearly)

##### DAILY GRAZING FILE (.DGZ) ################################################

## Importing and preparing weight data from the Baseline simulation's daily grazing file (.DGZ)
# In this simulation, grazing treatments were simulated together

path <- "C:/01-APEX1605_CO_baseline"

# Read baseline simulation's pastures data
dgz_noVar <- data.table::fread(file.path(path, "./WtGain_Simulation/APEX1605_CO_all20/CONUNN_TGM.DGZ"), 
                               fill = TRUE, skip = 9, header = TRUE) %>%
  mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM"))

# Filter and summarize pastures
weight_data_noVar_daily <- dgz_noVar %>%
  filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
  group_by(Date, Y, Treatment) %>% # summarizing by grazing treatment
  summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
  mutate(Simulation = "No variability")

## Function for pulling weight data from the .DGZ files of Variability folders

prepare_weight_data_dgz <- function(direct, simulation) {
  # Set working directory
  setwd(direct)
  
  # Read in daily grazing data
  dgz <- data.table::fread("./WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.DGZ", 
                           fill = TRUE, skip = 9, header = TRUE) %>%
    mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")))
  
  # Filter and summarize by grazing treament
  dgz_filtered <- dgz %>%
    filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
    distinct() %>% # removes duplicate rows (one for each plant)
    filter(`DWGkg/d` > 0) %>% # removes data from pastures that were rested (DWG == 0)
    left_join(PastureID, by = "ID") %>%
    group_by(Date, Y, Treatment) %>% 
    summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
    mutate(Simulation = simulation) # Add simulation column
  
}

weight_data_spatVar_daily <- prepare_weight_data_dgz(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div",
                                                     simulation = "Spatial Variability")

# weight_data_spatTempVar_daily <- prepare_weight_data_dgz(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop",
#                                                          simulation = "Spatial + Temporal Variability")

# Combining weight data in preparation for visualization of simulation differences
grazing_weight_data_daily <- rbind(weight_data_spatVar_daily,
                                   weight_data_noVar_daily
)

## Visualization of simulation differences


## Visualization of simulation differences by Treatment and variable, faceted by Year

# Daily Weight Gain - CARM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'CARM', ], aes(x = Date)) +
  geom_smooth(aes(y = DWG, color = Simulation), method = "gam") +
  theme_bw() +
  ylab("Daily Weight Gain (kg/hd/day) - CARM") +
  ggtitle("Daily Weight Gain (CARM)") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))

# Daily Weight Gain - TRM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'TRM', ], aes(x = Date)) +
  geom_smooth(aes(y = DWG, color = Simulation), method = "gam") +
  theme_bw() +
  ylab("Daily Weight Gain (kg/hd/day) - TRM") +
  ggtitle("Daily Weight Gain (TRM)") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))

# Daily Animal Weight - CARM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'CARM', ], aes(x = Date)) +
  geom_line(aes(y = GZWT, color = Simulation), linewidth = 0.75) +
  theme_bw() +
  ylab("Daily Animal Weight (kg/hd/day) - CARM") +
  ggtitle("Daily Animal Weight (CARM)") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))

# Daily Animal Weight - TRM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'TRM', ], aes(x = Date)) +
  geom_line(aes(y = GZWT, color = Simulation), linewidth = 0.75) +
  theme_bw() +
  ylab("Daily Animal Weight (kg/hd/day) - TRM") +
  ggtitle("Daily Animal Weight (TRM)") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))