prep_data <- function(path, simulation_type) {
  
  # Load observed data
  observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", 
                            header = TRUE) %>%
    select(2:13, 17) %>%
    mutate(CSAG = AG, WSPG = (BOBU + WSPG) / 2, CSPG = C3PG) %>%
    dplyr::rename(Year = YearSampled) %>%
    select(1:5, 9:15) %>%
    subset(Year %in% c(2014:2018)) %>%
    pivot_longer(cols = c(FORB:SS, CSAG:CSPG), 
                 names_to = "APEXcodeFG", values_to = "v1") %>%
    group_by(Year, Pasture, Plot, Transect, Treatment, APEXcodeFG) %>%
    summarize(v1 = mean(v1))
  
  observed_data_clean <- observed_data %>%
    group_by(Year, Pasture, Plot, Treatment, APEXcodeFG) %>%
    summarize(MeankgPerHa = mean(v1)) %>%
    filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
             Treatment %in% c("AGM", "TGM")) %>%
    mutate(Type = "Observed",
           Date = ymd(paste(Year, 8, 6, sep = "-")),
           WS = NA,
           APEXcodeFG = as.character(APEXcodeFG)) %>%
    ungroup(Year) %>%
    select(-Year) %>%
    filter(Treatment == "TGM")
  
  change_plant_codes <- function(df) {
    df %>%
      mutate(APEXcodeFG = gsub("SS", "SSHB", APEXcodeFG),
             APEXcodeFG = gsub("CSAG", "VUOC", APEXcodeFG),
             APEXcodeFG = gsub("FORB", "FRB3", APEXcodeFG),
             Treatment = gsub("TGM", "TRM", Treatment),
             Treatment = gsub("AGM", "CARM", Treatment),
             Pasture = gsub("NH", "10S", Pasture))
  }
  
  observed_data_clean <- change_plant_codes(observed_data_clean)
  
  import_and_filter_data <- function(path, years = c(2014:2018)) {
    fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
      filter(Y %in% years) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             ID = as.integer(ID)) %>%
      select(-c(Y, M, D))
  }
  
  clean_biomass_data <- function(simulated_biomass) {
    simulated_biomass %>%
      merge(PastureID, by = c("ID", "Treatment")) %>%
      mutate(Type = "Simulated",
             STL_kgha = STL * 1000,
             STD_kgha = STD * 1000,
             Biomass = STL_kgha + STD_kgha) %>%
      dplyr::rename(APEXcodeFG = CPNM,
                    MeankgPerHa = Biomass) %>%
      select(-c(STL, STD, STL_kgha, STD_kgha))
  }
  
  if (simulation_type == "variability") {
    simulated_biomass <- import_and_filter_data(path) %>%
      mutate(Treatment = "TRM")
    
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
    
    simulated_biomass_clean <- clean_biomass_data(simulated_biomass)
    
    filtered_simulated_biomass <- simulated_biomass_clean %>% filter(month(Date) == 8,
                                                                     day(Date) == 6) %>%
      filter(Treatment == "TRM")
    
    combined_data <- merge(filtered_simulated_biomass, observed_data_clean,
                           by = c("Pasture", "Plot","APEXcodeFG", "Date"))
    
  } else if (simulation_type == "no variability") {
    simulated_biomass <- import_and_filter_data(path) %>%
      mutate(Treatment = ifelse(ID %in% 1:10, "TRM", "CARM"))
    
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
      mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))
    
    simulated_biomass_clean <- clean_biomass_data(simulated_biomass)
    
    filtered_simulated_biomass <- simulated_biomass_clean %>% filter(month(Date) == 8,
                                                                     day(Date) == 6) %>%
      filter(Treatment == "TRM")
    
    observed_data_summarized <- observed_data %>%
      group_by(Year, Treatment, Pasture, APEXcodeFG) %>%
      dplyr::summarize(MeankgPerHa = mean(v1)) %>%
      subset(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
               Treatment %in% c("AGM", "TGM")) %>%
      mutate(Type = "Observed",
             Date = ymd(paste(Year, 8, 6, sep = "-")),
             WS = NA,
             APEXcodeFG = as.character(APEXcodeFG)) %>%
      ungroup(Year) %>%
      select(-Year)
    
    observed_data_summarized <- change_plant_codes(observed_data_summarized)
    
    observed_data_summarized$Pasture <- gsub("NH", "10S", observed_data_summarized$Pasture)
    
    combined_data <- merge(filtered_simulated_biomass, observed_data_summarized,
                           by = c("Pasture", "APEXcodeFG", "Date"))
  }
  
  # Additional data manipulation for output
  combined_data_clean <- combined_data %>%
    select(APEXcodeFG, MeankgPerHa.x, MeankgPerHa.y) %>%
    rename(Simulated = MeankgPerHa.x, Observed = MeankgPerHa.y) 
  
  return(combined_data_clean)
}

# Preparing data from each simulation:
spatVar <- prep_data("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag", "variability") %>%
  mutate(Sim.Type = "Spatial Variability")
spatTempVar <- prep_data("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag", "variability") %>%
  mutate(Sim.Type = "Spatial + Temporal Variability")
noVar <- prep_data("C:/01-APEX1605_CO_baseline/Cage Biomass Simulation/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag", "no variability") %>%
  mutate(Sim.Type = "No Variability")

# Combining comparison data
sim_obs_compare <- rbind(spatVar, spatTempVar, noVar)

sim_obs_compare$APEXcodeFG <- factor(sim_obs_compare$APEXcodeFG, 
                                     levels = c("WSPG", "CSPG", "FRB3", "SSHB", "VUOC"))

ggplot(sim_obs_compare, aes(x = Observed, y = Simulated, color = Sim.Type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = Sim.Type)) +
  facet_wrap(.~APEXcodeFG, ncol = 1, scales = "free") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.75)  +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        panel.spacing = unit(0.75, "lines")) +
  labs(x = expression(paste("Observed Standing Biomass (kg ", " ha" ^-1,")")),
       y = expression(paste("Simulated Standing Biomass (kg ", " ha" ^-1,")")),
       color = "Simulation")

# process_sim_plantStress <- function(path, year_range = c(2014:2018), 
#                                     species_codes = c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB"),
#                                     id_filter = c(1:10)) {
#   
#   import_plantStress <- fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
#     filter(Y %in% year_range, 
#            CPNM %in% species_codes,
#            ID %in% id_filter) %>%
#     select(ID, Y, M, D, CPNM, WS, TS) %>%
#     mutate(Date = ymd(paste(Y, M, D, sep = "-"))) %>%
#     select(-c(Y:D))
#   
#   PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv") %>%
#     filter(Treatment == "TRM")
#   
#   SimBiom_plantStress <- merge(PastureID, import_plantStress, by = "ID") %>%
#     group_by(Date, CPNM) %>%
#     summarize(WS_mean = mean(WS), TS_mean = mean(TS),
#               WS_SD = sd(WS), TS_SD = sd(TS))
#   
#   SimBiom_plantStress$CPNM <- gsub(pattern = "VUOC", replacement = "CSAG", x = SimBiom_plantStress$CPNM)
#   SimBiom_plantStress$CPNM <- gsub(pattern = "FRB3", replacement = "FORB", x = SimBiom_plantStress$CPNM)
#   
#   # Reshape the data to long format
#   plantStress_long <- SimBiom_plantStress %>%
#     pivot_longer(cols = c(WS_mean, TS_mean), 
#                  names_to = "Stress_Type", 
#                  values_to = "Value") %>%
#     mutate(Stress_Type = factor(Stress_Type, 
#                                 levels = c("WS_mean", "TS_mean"),
#                                 labels = c("Water Stress", "Temperature Stress")))
#   
#   return(plantStress_long)
# }
# 
# plantStress_var_allTRM <- process_sim_plantStress("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag",
#                                                   id_filter = c(1:46))
# 
# plantStress_var_pop_allTRM <- process_sim_plantStress("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag",
#                                                       id_filter = c(1:46))
# 
# ggplot(plantStress_var_allTRM, aes(x = Date, y = Value, 
#                                    color = Stress_Type, 
#                                    linetype = Stress_Type)) +
#   geom_line(aes(color = Stress_Type)) +  
#   scale_color_manual(values = c("Water Stress" = "steelblue2", 
#                                 "Temperature Stress" = "sienna3")) +
#   scale_linetype_manual(values = c("Water Stress" = "solid", 
#                                    "Temperature Stress" = "dashed")) +
#   facet_wrap(.~factor(CPNM, 
#                       levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")),
#              ncol = 1) +
#   theme_bw() +
#   theme(text = element_text(size = 15, family = 'serif'),
#         axis.text.x = element_text(angle = 90),
#         panel.spacing = unit(0.75, "lines")) +
#   labs(x = "Date",
#        y = "Plant Stress (0-1)",
#        color = "Stress Type Color",
#        linetype = "Stress Type Line") +  
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")
# 
# ggplot(plantStress_var_pop_allTRM, aes(x = Date, y = Value, 
#                                        color = Stress_Type, 
#                                        linetype = Stress_Type)) +
#   geom_line() +  
#   scale_color_manual(values = c("Water Stress" = "steelblue2", 
#                                 "Temperature Stress" = "sienna3")) +
#   scale_linetype_manual(values = c("Water Stress" = "solid", 
#                                    "Temperature Stress" = "dashed")) +
#   facet_wrap(.~factor(CPNM, 
#                       levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")),
#              ncol = 1) +
#   theme_bw() +
#   theme(text = element_text(size = 15, family = 'serif'),
#         axis.text.x = element_text(angle = 90),
#         panel.spacing = unit(0.75, "lines")) +
#   labs(x = "Date",
#        y = "Plant Stress (0-1)",
#        color = "Stress Type") +  
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")
