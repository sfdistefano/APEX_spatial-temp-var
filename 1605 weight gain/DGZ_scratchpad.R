## Importing and preparing weight data from the Baseline simulation's daily grazing file (.DGZ)
# In this simulation, grazing treatments were simulated together

path <- "C:/01-APEX1605_CO_baseline"

# Read baseline simulation's pastures data
dgz_noVar <- data.table::fread(file.path(path, "./WtGain_Simulation/APEX1605_CO_all20/CONUNN_TGM.DGZ"), 
                               fill = TRUE, skip = 9, header = TRUE) %>%
  mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")),
         Treatment = ifelse(HERD %in% c(1:10), "TRM", "CARM"))

# Filter and summarize pastures
weight_data_noVar_daily <- dgz_noVar %>%
  filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
  group_by(Date, Y, Treatment) %>% # summarizing by grazing treatment
  summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
  mutate(Simulation = "No variability")

## Importing data from scenario with spatial variability
setwd("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div")

# Read in daily grazing data
dgz <- data.table::fread("./WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.DGZ", 
                         fill = TRUE, skip = 9, header = TRUE) %>%
  mutate(Date = lubridate::ymd(paste(Y,M,D, sep = "-")))

# Filter and summarize by grazing treament
weight_data_spatVar_daily <- dgz %>%
  filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
  distinct() %>% # removes duplicate rows (one for each plant)
  filter(`DWGkg/d` > 0) %>% # removes data from pastures that were rested (DWG == 0)
  mutate(Treatment = ifelse(HERD %in% c(1:10), "TRM", "CARM")) %>%
  group_by(Date, Y, Treatment) %>% 
  summarize(DWG = mean(`DWGkg/d`), GZWT = mean(GZWTkg), .groups = 'drop') %>%
  mutate(Simulation = "Spatial Variability") # Add simulation column

# Combining weight data in preparation for visualization of simulation differences
grazing_weight_data_daily <- rbind(weight_data_spatVar_daily,
                                   weight_data_noVar_daily
)

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