library(tidyverse)
library(reshape2)
library(data.table)
library(ggsci)
library(hydroGOF)

##### ANNUAL GRAZING FILE (.AGZ) ###############################################

# Importing reference information for pastures
PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv") %>%
  filter(Treatment == "TRM")
PastureID_20sa <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

### Function to read and process simulated data by grazing treatment
prepare_weight_data_agz <- function(file_path, years, id_filter, sim_type) {
  data.table::fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years, ID %in% id_filter) %>%
    group_by(YR) %>%
    summarize(MeanWT = mean(WTGkg.hd.d, na.rm = TRUE)) %>%
    mutate(Type = sim_type, SD = 0) %>%
    rename(Year = YR)
}

## Importing observed annual mean weight gain data
observed_weight_data <- read.csv("C:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2023_SD.csv") %>%
  mutate(gain = ADG / 2.2046) %>%  # Converting from lbs to kg
  merge(PastureID_20sa, by = 'Pasture', relationship = "many-to-many", all.x = TRUE)

# Summarizing observed data for each treatment type
observed_weight_data_filtered <- observed_weight_data %>%
  select(Year, Treatment, gain) %>%
  filter(Year %in% c(2014:2018)) %>%
  group_by(Year, Treatment) %>%
  summarize(MeanWT = mean(gain, na.rm = TRUE), SD = sd(gain), Type = "Measured", .groups = 'drop')

# Filtering for traditional grazing (TGM) observed data
tgm_observed_weight_data <- observed_weight_data_filtered %>%
  filter(Treatment == "TGM") %>%
  select(-Treatment)

## Importing simulated data using the reusable function
years <- c(2014:2018)

# Baseline simulation scenario
tgm_weight_data_noVar_yearly <- prepare_weight_data_agz(
  "C:/01-APEX1605_CO_baseline/Wt Gain Simulation/APEX1605_CO_all20/CONUNN_TGM.AGZ",
  years, 1:10, "Simulated: no variability"
)

# Spatial variability simulation scenario
tgm_weight_data_spatVar_yearly <- prepare_weight_data_agz(
  "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ",
  years, 1:46, "Simulated: spatial variability"
)

# Spatial + temporal variability simulation scenario
tgm_weight_data_spatTempVar_yearly <- prepare_weight_data_agz(
  "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ",
  years, 1:46, "Simulated: spatial + temporal variability"
)

## Combining all traditional grazing observed and simulated data
tgm_weight_data_yearly <- rbind(
  tgm_observed_weight_data, tgm_weight_data_noVar_yearly, 
  tgm_weight_data_spatVar_yearly, tgm_weight_data_spatTempVar_yearly
)

# Reorder the levels of the Type column
tgm_weight_data_yearly$Type <- factor(tgm_weight_data_yearly$Type, 
                                      levels = c("Measured", 
                                                 "Simulated: no variability", 
                                                 "Simulated: spatial variability", 
                                                 "Simulated: spatial + temporal variability"))

# Create a bar plot with error bars
ggplot(tgm_weight_data_yearly, aes(x = factor(Year), y = MeanWT, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bars for mean weight
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars
  labs(x = "Year", 
       y = "Mean Daily Weight Gain (kg/head/day)", 
       fill = "Data Source") +  # Labels and legend title
  theme_bw() +   # Clean theme
  scale_fill_brewer(palette = "Set3") +
  theme(text = element_text(size = 15, family = 'serif'))

### Function to read and process simulated data by ecological site
prepare_weight_data_agz_ecosite <- function(file_path, years, sim_type) {
  data.table::fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years) %>%
    merge(PastureID, by = "ID") %>%
    group_by(YR, Ecosite) %>%
    summarize(MeanWT = mean(WTGkg.hd.d, na.rm = TRUE),
              SD = sd(WTGkg.hd.d)) %>%
    mutate(Type = sim_type) %>%
    rename(Year = YR)
}

# Spatial variability simulation scenario
tgm_weight_data_spatVar_ecosite <- prepare_weight_data_agz_ecosite("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ",
                                                                   years, "Simulated: spatial variability")
# Spatial + temporal variability simulation scenario
tgm_weight_data_spatTempVar_ecosite <- prepare_weight_data_agz_ecosite("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ",
                                                                       years, "Simulated: spatial + temporal variability")

# Combining both variability scenarios
tgm_weight_data_ecosite <- rbind(tgm_weight_data_spatVar_ecosite, tgm_weight_data_spatTempVar_ecosite)

# Reorder the Ecosite factor levels
tgm_weight_data_ecosite$Ecosite <- factor(tgm_weight_data_ecosite$Ecosite, 
                                          levels = c("Loamy","Salt Flats", "Sandy"))

# Reorder the Type factor levels
tgm_weight_data_ecosite$Type <- factor(tgm_weight_data_ecosite$Type, 
                                       levels = c("Simulated: spatial variability", 
                                                  "Simulated: spatial + temporal variability"))


# Get the Set3 palette colors
set3_colors <- brewer.pal(n = 12, name = "Set3")

# Select the 2nd and 3rd colors from Set3
selected_colors <- set3_colors[3:4]


## Visualizing differences in mean daily weight gain (yearly) between ecological sites
ggplot(tgm_weight_data_ecosite, aes(x = factor(Year), y = MeanWT, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars
  geom_text(aes(label = round(MeanWT, 2)),
            position = position_dodge(width = 0.9),
            vjust = -1.75, size = 4, family ='serif') +  # Adjust padding around the text
  facet_wrap(.~Ecosite, ncol = 1) +
  labs(x = "Year", 
       y = "Mean Daily Weight Gain (kg/head/day)", 
       fill = "Data Source") +  # Labels and legend title
  theme_bw() +   # Clean theme
  theme(text = element_text(size = 15, family = 'serif')) +
  scale_fill_manual(values = selected_colors)

##### DAILY GRAZING FILE (.DGZ) ################################################

## Importing and preparing weight data from the Baseline simulation's daily grazing file (.DGZ)
# In this simulation, grazing treatments were simulated together

path <- "C:/01-APEX1605_CO_baseline"

# Read baseline simulation's pastures data
dgz_noVar <- data.table::fread(file.path(path, "./Wt Gain Simulation/APEX1605_CO_all20/CONUNN_TGM.DGZ"), 
                             fill = TRUE, skip = 9, header = TRUE) %>%
  mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TGM", "AGM"))

# Filter and summarize pastures
dgz_filtered_noVar <- dgz_noVar %>%
  filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
  group_by(Date, Y, Treatment) %>% # summarizing by grazing treatment
  summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
  mutate(Simulation = "No variability")

weight_data_noVar_daily <- dgz_filtered_noVar %>%
  split(.$Treatment) %>%
  lapply(function(df) df %>% select(-Treatment))

# Filter for grazing treatment of interest (traditional grazing management)
tgm_weight_data_noVar_daily <- weight_data_noVar_daily[['TGM']]

## Function for pulling weight data from the .DGZ files of Variability folders
# In these simulations, grazing treatments were simulated separately

prepare_weight_data_dgz <- function(direct, simulation) {
  # Set working directory
  setwd(direct)
  
  # Read traditionally grazed pastures data
  tgm_dgz <- data.table::fread("./Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.DGZ", 
                               fill = TRUE, skip = 9, header = TRUE) %>%
    mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")))
  
  # Read rotationally (adaptively) grazed pastures data
  agm_dgz <- data.table::fread("./Wt Gain Simulation/APEX1605_CO_AGM/CONUNN_TGM.DGZ",
                               fill = TRUE, skip = 9, header = TRUE) %>%
    mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")))
  
  # Filter and summarize traditionally grazed pastures
  tgm_dgz_filtered <- tgm_dgz %>%
    filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
    group_by(Date, Y) %>% # summarizing all TGM herds (n = 10) into 1 value/day
    summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
    mutate(Simulation = simulation) # Add simulation column
  
  # Filter and summarize rotationally grazed pastures
  agm_dgz_filtered <- agm_dgz %>%
    filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
    group_by(Date, Y) %>% # summarizing all AGM herds (n = 2) into 1 value/day
    summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
    mutate(Simulation = simulation) # Add simulation column
  
  # Return filtered datasets as a list
  return(list(TGM = tgm_dgz_filtered, AGM = agm_dgz_filtered))
}

weight_data_spatVar_daily <- prepare_weight_data_dgz(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div",
                                   simulation = "Spatial Variability")

weight_data_spatTempVar_daily <- prepare_weight_data_dgz(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop",
                                               simulation = "Spatial + Temporal Variability")

# Filter for grazing treatment of interest (traditional grazing management)
tgm_weight_data_spatVar_daily <- weight_data_spatVar_daily[['TGM']]

tgm_weight_data_spatTempVar_daily <- weight_data_spatTempVar_daily[['TGM']]

# Combining TGM weight data in preparation for visualization of simulation differences
tgm_weight_data_daily <- rbind(tgm_weight_data_spatVar_daily, 
                               tgm_weight_data_spatTempVar_daily, 
                               tgm_weight_data_noVar_daily
                               )

## Visualization of simulation differences

# Daily Weight Gain
ggplot(tgm_weight_data_daily, aes(x = Date)) +
  geom_smooth(aes(y = DWG, color = Simulation), method = "gam") +
  theme_bw() +
  ylab("Daily Weight Gain (kg/hd/day)") +
  facet_wrap(~Y, scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))

# Daily Animal Weight
ggplot(tgm_weight_data_daily, aes(x = Date)) +
  geom_line(aes(y = GZWT, color = Simulation), linewidth = 0.75) +
  theme_bw() +
  ylab("Daily Animal Weight (kg/hd/day)") +
  facet_wrap(~Y, scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))
