prepare_weight_data_dgz_ecositeTRM <- function(direct, simulation) {
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
    # filter(`DWGkg/d` > 0) %>% # removes data from pastures that were rested (DWG == 0)
    left_join(PastureID, by = "ID") %>%
    # filter(Treatment == "TRM") %>%
    group_by(Date, Y, Ecosite, Treatment) %>% 
    summarize(DWG = mean(`DWGkg/d`), 
              GZWT = mean(GZWTkg), 
              TDN = mean(TDN)) %>%
    mutate(Simulation = simulation) # Add simulation column
  
}

weight_data_spatVar_ecositeTRM <- prepare_weight_data_dgz_ecositeTRM(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div",
                                                     simulation = "Spatial Variability") %>%
  filter(Treatment == "TRM")

weight_data_spatVar_ecositeCARM <- prepare_weight_data_dgz_ecositeTRM(direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div",
                                                                     simulation = "Spatial Variability") %>%
  filter(Treatment == "CARM")

# Total Digestible Nutrients - CARM
ggplot(weight_data_spatVar_ecositeCARM, aes(x = Date)) +
  geom_line(aes(y = TDN, color = Ecosite)) +
  theme_bw() +
  ylab("TDN - CARM") +
  ggtitle("Total Digestible Nutrients (CARM) by Ecosite") +
  facet_wrap(~ year(Date), scales = "free_x", ncol = 1) +
  theme(text = element_text(size = 15, family = 'serif'))


# Daily Animal Weight - TRM
ggplot(weight_data_spatVar_ecositeTRM, aes(x = Date)) +
  geom_line(aes(y = DWG, color = Ecosite), linewidth = 0.75) +
  theme_bw() +
  ylab("Daily Animal Weight (kg/hd/day) - TRM") +
  ggtitle("Daily Animal Weight (TRM) by Ecological Site") +
  facet_wrap(~ year(Date), scales = "free_x", ncol = 1) +
  theme(text = element_text(size = 15, family = 'serif'))
