# Filter and summarize pastures
weight_data_noVar_daily <- dgz_noVar %>%
  filter(M %in% c(5:9), Y %in% c(2014:2018)) %>%
  group_by(Date, Y, Treatment) %>% # summarizing by grazing treatment
  summarize(DWG = mean(`DWGkg/d`), 
            GZWT = mean(GZWTkg), 
            TDN = mean(TDN),
            .groups = 'drop') %>%
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
    summarize(DWG = mean(`DWGkg/d`), 
              GZWT = mean(GZWTkg), 
              TDN = mean(TDN),
              .groups = 'drop') %>%
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

# Total Digestible Nutrients - CARM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'CARM', ], aes(x = Date)) +
  geom_line(aes(y = TDN, color = Simulation)) +
  theme_bw() +
  ylab("TDN - CARM") +
  ggtitle("Total Digestible Nutrients (CARM)") +
  facet_wrap(~ year(Date), scales = "free_x", ncol = 1) +
  theme(text = element_text(size = 15, family = 'serif'))

# Total Digestible Nutrients - TRM
ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'TRM', ], aes(x = Date)) +
  geom_line(aes(y = TDN, color = Simulation)) +
  theme_bw() +
  ylab("TDN - TRM") +
  ggtitle("Total Digestible Nutrients (TRM)") +
  # facet_wrap(~ year(Date), scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif'))