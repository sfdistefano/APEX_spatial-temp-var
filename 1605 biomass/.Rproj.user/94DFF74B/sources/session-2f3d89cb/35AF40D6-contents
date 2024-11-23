library(tidyverse)
library(tidyr)
library(data.table)
library(hydroGOF)

###### SUMMARIZING BY GRAZING TREATMENT ########################################

#### IMPORT & CLEAN DATA 
bm_cag_combine <- function(num.sa = 20, direct, division = "Pasture"){
  
  setwd(direct)
  
  # Helper function for importing data
  import_and_filter_data <- function(path, years = c(2014:2018)) {
    fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
      filter(Y %in% years) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             ID = as.integer(ID)) %>%
      select(-c(Y:M, D))
  }
  
  # Helper function for data cleaning
  clean_biomass_data <- function(simulated_biomass) {
    simulated_biomass %>%
      merge(PastureID, by = c("ID", "Treatment")) %>%
      mutate(Type = "Simulated",
             STL_kgha = STL * 1000,
             STD_kgha = STD * 1000,
             Biomass = STL_kgha + STD_kgha) %>%
      dplyr::rename(APEXcodeFG = CPNM,
                    MeankgPerHa = Biomass) %>%
      select(-c(STL:STD), -c(STL_kgha:STD_kgha)) 
  }
  
  # Helper function for changing plant codes to match between observational and simulated data
  change_plant_codes <- function(df) {
    df %>%
      mutate(APEXcodeFG = gsub("SS", "SSHB", APEXcodeFG),
             APEXcodeFG = gsub("CSAG", "VUOC", APEXcodeFG),
             APEXcodeFG = gsub("FORB", "FRB3", APEXcodeFG),
             Treatment = gsub("TGM", "TRM", Treatment),
             Treatment = gsub("AGM", "CARM", Treatment),
             Pasture = gsub("NH", "10S", Pasture))
  }
  
  ## Import observed data
  observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", 
                            header = T) %>%
    select(2:13,17) %>%
    mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
    dplyr::rename(Year = YearSampled) %>%
    select(1:5,9:15) %>%
    subset(Year %in% c(2014:2018)) %>%
    pivot_longer(cols = c(FORB:SS, CSAG:CSPG), 
                 names_to = "APEXcodeFG", values_to = "v1") %>%
    group_by(Year,Pasture,Plot, Transect, Treatment,APEXcodeFG) %>%
    summarize(v1 = mean(v1))
  
  if(num.sa == 20){
    
    path <- "./Cage Biomass Simulation/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"
    
    ## Importing simulated data
    simulated_biomass <- import_and_filter_data(path) %>%
      mutate(Treatment = ifelse(ID %in% 1:10, "TRM", "CARM"))
    
    ## Importing reference information
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
      mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))
    
    ## Cleaning simulated data
    simulated_biomass02 <- clean_biomass_data(simulated_biomass)
    
    ## Summarizing observed data
    observed_data_summarized <- observed_data %>%
      group_by(Year,Pasture,Plot, Transect, Treatment,APEXcodeFG) %>%
      summarize(v1 = mean(v1)) %>%
      group_by(Year,Pasture,Plot,Treatment,APEXcodeFG) %>%
      summarize(v1 = mean(v1)) %>%
      group_by(Year,Pasture,Treatment,APEXcodeFG) %>%
      dplyr::summarize(MeankgPerHa = mean(v1)) %>%
      filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
               Treatment %in% c("AGM","TGM")) %>%
      mutate(Type = "Observed",
             Date = ymd(paste(Year, 8, 6, sep = "-")),
             WS = NA,
             APEXcodeFG=as.character(APEXcodeFG)) %>%
      ungroup(Year) %>%
      select(-Year)
    
    # Changing plant codes
    observed_data_summarized <- change_plant_codes(observed_data_summarized)
    
    # changing one pasture name
    observed_data_summarized$Pasture <- gsub("NH", "10S", observed_data_summarized$Pasture)
    
    ## Adding pasture information
    observed_data_summarized <- merge(observed_data_summarized, PastureID, by = c("Pasture", "Treatment"))
    
    ## Summarizing data into pasture
    Biomass <- rbind(observed_data_summarized, simulated_biomass02) %>%
      group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
      dplyr::summarise(MeankgPerHa = mean(MeankgPerHa)) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(Mean = mean(MeankgPerHa))
    
    Biomass_se <- rbind(observed_data_summarized, simulated_biomass02) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)),
                       SD = sd(MeankgPerHa))
    
  } else if(num.sa == 92) {
    
    pathAGM <- "./Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag"
    pathTRM <- "./Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"
    
    # Importing simulated data
    simulated_biomass_AGM <- import_and_filter_data(pathAGM) %>%
      mutate(Treatment = "CARM")
    
    simulated_biomass_TRM <- import_and_filter_data(pathTRM) %>%
      mutate(Treatment = "TRM")
    
    ## Importing reference information
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
    
    ## Cleaning reference information
    simulated_biomass02_AGM <- clean_biomass_data(simulated_biomass_AGM)
    
    simulated_biomass02_TRM <- clean_biomass_data(simulated_biomass_TRM)
    
    if(division == "Pasture") {
      
      ## Summarizing observational data
      observed_data_summarized <- observed_data %>%
        group_by(Year,Treatment, Pasture,Plot,APEXcodeFG) %>%
        dplyr::summarize(MeankgPerHa = mean(v1)) %>%
        subset(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
                 Treatment %in% c("AGM","TGM"))%>%
        mutate(Type = "Observed",
               Date = ymd(paste(Year, 8, 6, sep = "-")),
               WS = NA,
               APEXcodeFG=as.character(APEXcodeFG)) %>%
        ungroup(Year) %>%
        select(-Year)
      
      # Changing plant codes
      observed_data_summarized <- change_plant_codes(observed_data_summarized)
      
      # changing one pasture name
      observed_data_summarized$Pasture <- gsub("NH", "10S", observed_data_summarized$Pasture)
      
      ## Adding pasture information
      observed_data_summarized <- merge(observed_data_summarized, PastureID, by = c("Pasture","Plot","Treatment"))
      
      ## Summarizing data into pasture
      Biomass <- rbind(observed_data_summarized, simulated_biomass02_AGM, simulated_biomass02_TRM) %>%
        group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
        dplyr::summarise(MeankgPerHa = mean(MeankgPerHa)) %>%
        group_by(Date,Treatment,Type,APEXcodeFG) %>%
        dplyr::summarise(Mean = mean(MeankgPerHa))
      
      Biomass_se <- rbind(observed_data_summarized, simulated_biomass02_AGM, simulated_biomass02_TRM) %>%
        group_by(Date,Treatment,Type,APEXcodeFG) %>%
        dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)),
                         SD = sd(MeankgPerHa))
      
    } else if (division == "Ecosite") {
      ## Cleaning observational data
      observed_data_clean <- observed_data %>%
        group_by(Year,Pasture,Plot,Treatment,APEXcodeFG) %>%
        summarize(MeankgPerHa = mean(v1)) %>%
        filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
                 Treatment %in% c("AGM","TGM")) %>%
        mutate(Type = "Observed",
               Date = ymd(paste(Year, 8, 6, sep = "-")),
               WS = NA,
               APEXcodeFG=as.character(APEXcodeFG)) %>%
        ungroup(Year) %>%
        select(-Year)
      
      # Changing plant codes
      observed_data_clean <- change_plant_codes(observed_data_clean)
      
      # changing one pasture name
      observed_data_clean$Pasture <- gsub("NH", "10S", observed_data_clean$Pasture)
      
      ## Adding pasture information
      observed_data_clean <- merge(observed_data_clean, PastureID, by = c("Pasture", "Treatment", "Plot"))
      
      ## Summarizing data into pasture
      Biomass <- rbind(observed_data_clean, simulated_biomass02_AGM, simulated_biomass02_TRM) %>%
        group_by(Date, Ecosite, Treatment, Type, APEXcodeFG) %>%
        dplyr::summarise(Mean = mean(MeankgPerHa)) 
      
      Biomass_se <- rbind(observed_data_clean, simulated_biomass02_AGM, simulated_biomass02_TRM) %>%
        group_by(Date,Ecosite,Type,APEXcodeFG) %>%
        dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)),
                         SD = sd(MeankgPerHa))
    } else {
      stop("Invalid division type. Choose 'Pasture' or 'Ecosite'")
    }
  } else {
    stop("incompatible number of subareas")
  }
  
  Biomass <- merge(Biomass, Biomass_se)
  
  Biomass$APEXcodeFG <- gsub("FRB3","FORB",Biomass$APEXcodeFG)
  Biomass$APEXcodeFG <- gsub("VUOC","CSAG",Biomass$APEXcodeFG)
  
  Biomass <- Biomass %>% 
    filter(APEXcodeFG %in% c("CSPG", "FORB", "CSAG", "WSPG", "SSHB"))
  
  return(Biomass)
}

## Baseline
Biomass_base_graze <- bm_cag_combine(num.sa = 20, 
                                     direct = "C:/01-APEX1605_CO_baseline/")  %>% 
  mutate(Sim.Type = "Simulated: no variability") %>%
  filter(Treatment == "TRM")

## Spatial Variability
Biomass_var_graze <- bm_cag_combine(num.sa = 92,
                                    direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div") %>%
  mutate(Sim.Type = "Simulated: spatial variability") %>%
  filter(Treatment == "TRM")

## Spatial + Temporal Variability
Biomass_var_pop_graze <- bm_cag_combine(num.sa = 92,
                                        direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop") %>%
  mutate(Sim.Type = "Simulated: spatial+temporal variability") %>%
  filter(Treatment == "TRM")

#### COMPARE ACROSS SCENARIOS @ GRAZING TREATMENT LEVEL 
Biomass_comb_graze <- rbind(Biomass_base_graze,
                            Biomass_var_graze,
                            Biomass_var_pop_graze
) 

## Simulation stats
graze_compare <- Biomass_comb_graze %>%
  select(-SE, -SD) %>%
  filter(month(Date) == 8, day(Date) == 6) %>%
  pivot_wider(names_from = Type, values_from = Mean)

graze_simStats <- graze_compare %>%
  group_by(Treatment, Sim.Type, APEXcodeFG) %>%
  summarise(rmse = round(rmse(Simulated, Observed), 2),
            nrmse = round(nrmse(Simulated, Observed, norm = "maxmin")/100, 2),
            d = round(d(Simulated, Observed), 2),
            pbias = pbias(Simulated, Observed, dec = 2))

# write.csv(graze_simStats, "C:/APEX data and scripts/APEX outputs/grazeTrt_simStats_10012024.csv")

#### SUMMARIZING BY ECOLOGICAL SITE ############################################
## Spatial Variability
Biomass_var_ecosite <- bm_cag_combine(num.sa = 92,
                                    direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div",
                                    division = "Ecosite") %>%
  mutate(Sim.Type = "Simulated: spatial variability") %>%
  filter(Treatment == "TRM")

## Spatial + Temporal Variability
Biomass_var_pop_ecosite <- bm_cag_combine(num.sa = 92,
                                        direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop",
                                        division = "Ecosite") %>%
  mutate(Sim.Type = "Simulated: spatial+temporal variability") %>%
  filter(Treatment == "TRM")

# ### COMPARE ACROSS SCENARIOS @ GRAZING TREATMENT LEVEL 
Biomass_comb_ecosite <- rbind(Biomass_var_ecosite,
                            Biomass_var_pop_ecosite) 

## Simulation stats
ecosite_compare <- Biomass_comb_ecosite %>%
  select(-SE, -SD) %>%
  filter(month(Date) == 8, day(Date) == 6) %>%
  pivot_wider(names_from = Type, values_from = Mean)

ecosite_simStats <- ecosite_compare %>%
  group_by(Ecosite, Sim.Type, APEXcodeFG) %>%
  summarise(rmse = round(rmse(Simulated, Observed), 2),
            nrmse = round(nrmse(Simulated, Observed, norm = "maxmin")/100, 2),
            d = round(d(Simulated, Observed), 2),
            pbias = pbias(Simulated, Observed, dec = 2))

# write.csv(ecosite_simStats, "C:/APEX data and scripts/APEX outputs/ecosite_simStats_10042024.csv")

#### VISUALIZING BIOMASS OUTCOMES ##############################################
## color blind friendly palette
cbbPalette <- c("#000000", "#e41a1c", "#377eb8", "#4daf4a")

## Plot across grazing treatment*plant functional group
ggplot()+
  geom_ribbon(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD),
              subset(Biomass_base_graze, Type %in% "Simulated"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = Mean, color = Sim.Type),
            subset(Biomass_comb_graze, Type %in% "Simulated"),
            linewidth = 1) +
  geom_point(aes(x = Date, y = Mean, color = "Observed"),
             subset(Biomass_base_graze, Type == "Observed"),
             size = 1.75, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = pmax(Mean - SD, 0), ymax = Mean + SD,
                    color = "Observed"),
                subset(Biomass_base_graze, Type %in% "Observed"),
                linewidth = 0.7, width = 100) +
  facet_grid(factor(APEXcodeFG, 
                    levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG"))~., 
             scales = "free_y") +
  ylab(expression(paste("Ungrazed Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_color_manual(values = cbbPalette,
                     name = "Forage Production Data") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

## color blind friendly palette
cbbPalette_v02 <- c("#000000", "#377eb8", "#4daf4a")

## Plot across ecological site*plant functional group
ggplot()+
  geom_ribbon(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD),
              subset(Biomass_var_ecosite, Type %in% "Simulated"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = Mean, color = Sim.Type),
            subset(Biomass_comb_ecosite, Type %in% "Simulated"),
            linewidth = 1) +
  geom_point(aes(x = Date, y = Mean, color = "Observed"),
             subset(Biomass_var_ecosite, Type == "Observed"),
             size = 1.75, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = pmax(Mean - SD, 0), ymax = Mean + SD,
                    color = "Observed"),
                subset(Biomass_var_ecosite, Type %in% "Observed"),
                linewidth = 0.7, width = 100) +
  facet_grid(factor(APEXcodeFG, 
                    levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")) ~ Ecosite, 
             scales = "free_y") +
  ylab(expression(paste("Ungrazed Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_color_manual(values = cbbPalette_v02,
                     name = "Forage Production Data") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

###### SUMMARIZING PLANT STRESS BY ECOLOGICAL SITE #############################
process_sim_plantStress <- function(path, year_range = c(2014:2018), 
                                    species_codes = c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB"),
                                    id_filter = c(1:10)) {
  
  import_plantStress <- fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
    filter(Y %in% year_range, 
           CPNM %in% species_codes,
           ID %in% id_filter) %>%
    select(ID, Y, M, D, CPNM, WS, TS) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-"))) %>%
    select(-c(Y:D))
  
  PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv") %>%
    filter(Treatment == "TRM")
  
  SimBiom_plantStress <- merge(PastureID, import_plantStress, by = "ID") %>%
    group_by(Date, Ecosite, CPNM) %>%
    summarize(WS_mean = mean(WS), TS_mean = mean(TS),
              WS_SD = sd(WS), TS_SD = sd(TS))
  
  SimBiom_plantStress$CPNM <- gsub(pattern = "VUOC", replacement = "CSAG", x = SimBiom_plantStress$CPNM)
  SimBiom_plantStress$CPNM <- gsub(pattern = "FRB3", replacement = "FORB", x = SimBiom_plantStress$CPNM)
  
  # Reshape the data to long format
  plantStress_long <- SimBiom_plantStress %>%
    pivot_longer(cols = c(WS_mean, TS_mean), 
                 names_to = "Stress_Type", 
                 values_to = "Value") %>%
    mutate(Stress_Type = factor(Stress_Type, 
                                levels = c("WS_mean", "TS_mean"),
                                labels = c("Water Stress", "Temperature Stress")))
  
  return(plantStress_long)
}

# plantStress_var_ecosite <- process_sim_plantStress("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag",
#                                                    id_filter = c(1:46))
# 
# plantStress_var_pop_ecosite <- process_sim_plantStress("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop/Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag",
#                                                        id_filter = c(1:46))
# 
# ggplot(plantStress_var_ecosite, aes(x = Date, y = Value, 
#                                     color = Stress_Type, 
#                                     linetype = Stress_Type)) +
#   geom_line(aes(color = Stress_Type)) +  
#   scale_color_manual(values = c("Water Stress" = "steelblue2", 
#                                 "Temperature Stress" = "sienna3")) +
#   scale_linetype_manual(values = c("Water Stress" = "solid", 
#                                    "Temperature Stress" = "dashed")) +
#   facet_grid(factor(CPNM, 
#                     levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")) ~ Ecosite) +
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
# ggplot(plantStress_var_pop_ecosite, aes(x = Date, y = Value, 
#                                         color = Stress_Type, 
#                                         linetype = Stress_Type)) +
#   geom_line(aes(color = Stress_Type)) +  
#   scale_color_manual(values = c("Water Stress" = "steelblue2", 
#                                 "Temperature Stress" = "sienna3")) +
#   scale_linetype_manual(values = c("Water Stress" = "solid", 
#                                    "Temperature Stress" = "dashed")) +
#   facet_grid(factor(CPNM, 
#                     levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")) ~ Ecosite) +
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
# ## Summarizing data by year for each plant stress type
# plantStress_var_ecosite_yearly <- plantStress_var_ecosite %>%
#   group_by(year(Date), Ecosite, Stress_Type) %>%
#   summarize(yearly_mean_stress = mean(Value)) %>%
#   mutate(Type = "Spatial Variability")
# 
# plantStress_var_pop_ecosite_yearly <- plantStress_var_pop_ecosite %>%
#   group_by(year(Date), Ecosite, Stress_Type) %>%
#   summarize(yearly_mean_stress = mean(Value)) %>%
#   mutate(Type = "Spatial + Temporal Variability")
# 
# plantStress_ecosite_yearly <- rbind(plantStress_var_ecosite_yearly, plantStress_var_pop_ecosite_yearly)

# write.csv(plantStress_ecosite_yearly, "C:/APEX data and scripts/APEX outputs/plantStress_yearly_mean.csv")