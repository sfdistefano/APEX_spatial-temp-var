## Code written by Sean Di Stefano 03/2024, altered from code written by Gong Cheng

library(tidyverse)
library(reshape2)
library(data.table)
library(hydroGOF)

name.cag <- c("N", "ID", "Y", "M", "D", "CPNM", "WSAha","Grazed.1", "Grazed.2",
              "Grazed.3","HUI", "LAI", "RD", "RW","BIOM", "STL", "CPHT", "STD", 
              "STDL", "GZSL", "GZSD", "A_DDM", "PRCP", "PET", "AET", "AT", "AE",
              "Q", "AT.SP", "WS", "NS", "TS", "MIN_STRESS", "SURF_LIT", 
              "TOTAL_LIT", "POP", "STL_N", "STD_N")

###### GRAZING TREATMENT #######################################################
#### IMPORT & CLEAN DATA 
bm_cag_combine <- function(num.sa = 20, direct){
  
  setwd(direct)
  
  if(num.sa == 20){
    
    path <- "./APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"
    
    ## Importing simulated data
    SimBiom <- fread(path, header = FALSE, skip = 2, 
                     col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
      select(-c(Y:D))
    
    ## Importing reference information
    PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
      mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))
    
    ## Cleaning simulated data
    SimBiom02 <- merge(SimBiom, PastureID,
                       by = c("ID", "Treatment")) %>%
      mutate(Type = "Simulated",
             STL_kgha = STL * 1000,
             STD_kgha = STD * 1000,
             Biomass = STL_kgha + STD_kgha) %>%
      dplyr::rename(APEXcodeFG = CPNM,
                    MeankgPerHa = Biomass)  %>%
      select(-c(STL:STD), -c(STL_kgha:STD_kgha))
    
    ## Importing observational data
    obv22 <- read.csv("D:/APEX data and scripts/Data/CPER Biomass/AGM_Biomass_Widecln_attr_2023-07-18.csv", header = T)%>%
      select(2:13,17) %>%
      mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
      dplyr::rename(Year = YearSampled) %>%
      select(1:5,9:15) %>%
      subset(Year %in% c(2014:2022)) %>%
      pivot_longer(cols = c(FORB:SS, CSAG:CSPG), names_to = "APEXcodeFG", values_to = "v1") %>%
      group_by(Year,Pasture,Plot, Transect, Treatment,APEXcodeFG) %>%
      summarize(v1 = mean(v1)) %>%
      group_by(Year,Pasture,Plot,Treatment,APEXcodeFG) %>%
      summarize(v1 = mean(v1)) %>%
      group_by(Year,Pasture,Treatment,APEXcodeFG) %>%
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
    obv22$APEXcodeFG <- gsub("SS","SSHB",obv22$APEXcodeFG)
    obv22$APEXcodeFG <- gsub("CSAG","VUOC",obv22$APEXcodeFG)
    obv22$APEXcodeFG <- gsub("FORB","FRB3",obv22$APEXcodeFG)
    obv22$Treatment<-gsub("TGM","TRM",obv22$Treatment)
    obv22$Treatment<-gsub("AGM","CARM",obv22$Treatment)
    
    # changing one pasture name
    obv22$Pasture <- gsub("NH", "10S", obv22$Pasture)
    
    ## Adding pasture information
    obv22 <- merge(obv22, PastureID, by = c("Pasture", "Treatment"))
    
    ## Summarizing data into pasture
    Biomass_pasture <- rbind(obv22, SimBiom02) %>%
      group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
      dplyr::summarise(MeankgPerHa = mean(MeankgPerHa)) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(Mean = mean(MeankgPerHa))
    
    Biomass_pasture_se <- rbind(obv22, SimBiom02) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)))
    
  } else if(num.sa == 92) {
    
    pathAGM <- "./APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag"
    pathTRM <- "./APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"
    
    # Importing simulated data
    SimBiom_AGM <- fread(pathAGM, header = FALSE, skip = 2,
                         col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = "CARM") %>%
      select(-c(Y:D))
    
    SimBiom_TRM <- fread(pathTRM, header = FALSE, skip = 2,
                         col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = "TRM") %>%
      select(-c(Y:D))
    
    ## Importing reference information
    PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
    
    ## Cleaning reference information
    SimBiom02_AGM <- merge(SimBiom_AGM, PastureID,
                           by = c("ID", "Treatment")) %>%
      mutate(Type = "Simulated",
             STL_kgha = STL * 1000,
             STD_kgha = STD * 1000,
             Biomass = STL_kgha + STD_kgha) %>%
      dplyr::rename(APEXcodeFG = CPNM,
                    MeankgPerHa = Biomass) %>%
      select(-c(STL:STD), -c(STL_kgha:STD_kgha))
    
    SimBiom02_TRM <- merge(SimBiom_TRM, PastureID,
                           by = c("ID", "Treatment")) %>%
      mutate(Type = "Simulated",
             STL_kgha = STL * 1000,
             STD_kgha = STD * 1000,
             Biomass = STL_kgha + STD_kgha) %>%
      dplyr::rename(APEXcodeFG = CPNM,
                    MeankgPerHa = Biomass) %>%
      select(-c(STL:STD), -c(STL_kgha:STD_kgha))
    
    ## Importing observational data
    obv22 <- read.csv("D:/APEX data and scripts/Data/CPER Biomass/AGM_Biomass_Widecln_attr_2023-07-18.csv", header = T)%>%
      select(2:13,17) %>%
      mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
      dplyr::rename(Year = YearSampled) %>%
      select(1:5,9:15) %>%
      subset(Year %in% c(2014:2022)) %>%
      pivot_longer(cols = c(FORB:SS, CSAG:CSPG), names_to = "APEXcodeFG", values_to = "v1") %>%
      group_by(Year,Pasture,Plot, Transect, Treatment,APEXcodeFG) %>%
      summarize(v1 = mean(v1)) %>%
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
    obv22$APEXcodeFG <- gsub("SS","SSHB",obv22$APEXcodeFG)
    obv22$APEXcodeFG <- gsub("CSAG","VUOC",obv22$APEXcodeFG)
    obv22$APEXcodeFG <- gsub("FORB","FRB3",obv22$APEXcodeFG)
    obv22$Treatment<-gsub("TGM","TRM",obv22$Treatment)
    obv22$Treatment<-gsub("AGM","CARM",obv22$Treatment)
    
    # changing one pasture name
    obv22$Pasture <- gsub("NH", "10S", obv22$Pasture)
    
    ## Adding pasture information
    obv22 <- merge(obv22, PastureID, by = c("Pasture","Plot","Treatment"))
    
    ## Summarizing data into pasture
    Biomass_pasture <- rbind(obv22, SimBiom02_AGM, SimBiom02_TRM) %>%
      group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
      dplyr::summarise(MeankgPerHa = mean(MeankgPerHa)) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(Mean = mean(MeankgPerHa))
    
    Biomass_pasture_se <- rbind(obv22, SimBiom02_AGM, SimBiom02_TRM) %>%
      group_by(Date,Treatment,Type,APEXcodeFG) %>%
      dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)))
    
  } else{
    print("incompatible number of subareas")
  }
  
  Biomass_pasture <- merge(Biomass_pasture, Biomass_pasture_se)
  
  Biomass_pasture$APEXcodeFG <- gsub("FRB3","FORB",Biomass_pasture$APEXcodeFG)
  Biomass_pasture$APEXcodeFG <- gsub("VUOC","CSAG",Biomass_pasture$APEXcodeFG)
  
  Biomass_pasture <- Biomass_pasture %>% filter(APEXcodeFG %in% c("CSPG", "FORB", "CSAG", "WSPG"))
  
  return(Biomass_pasture)
}

## Baseline
Biomass_base_graze <- bm_cag_combine(num.sa = 20, 
                                     direct = "D:/01-APEX1605_CO_baseline/")  %>% 
  mutate(Sim.Type = "Baseline") 

## Temporal Variability
Biomass_pop_graze <- bm_cag_combine(num.sa = 20, 
                                    direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_20 subareas_dyn plant pop") %>% 
  mutate(Sim.Type = "Temporal") 

## Spatial Variability
Biomass_var_graze <- bm_cag_combine(num.sa = 92, 
                                    direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div") %>% 
  mutate(Sim.Type = "Spatial")

## Spatial + Temporal Variability
Biomass_var_pop_graze <- bm_cag_combine(num.sa = 92, 
                                        direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop") %>% 
  mutate(Sim.Type = "Spatial+Temporal") 

### COMPARE ACROSS SCENARIOS @ GRAZING TREATMENT LEVEL 
Biomass_comb_graze <- rbind(Biomass_base_graze, Biomass_var_graze, 
                            Biomass_pop_graze, Biomass_var_pop_graze)

## Simulation stats
graze_compare <- Biomass_comb_graze %>% 
  select(-SE) %>%
  filter(month(Date) == 8, day(Date) == 6) %>%
  pivot_wider(names_from = Type, values_from = Mean) 

graze_simStats <- graze_compare %>%
  group_by(Treatment, Sim.Type, APEXcodeFG) %>%
  summarise(rmse = round(rmse(Simulated, Observed), 2),
            nrmse = nrmse(Simulated, Observed, norm = "maxmin")/100,
            d = round(d(Simulated, Observed), 3),
            pbias = pbias(Simulated, Observed))

# write.csv(graze_simStats, "D:/APEX data and scripts/APEX outputs/graze trt_simStats_04112024.csv")

## color blind friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Plot across grazing treatment*plant functional group
ggplot()+
  geom_line(aes(x = Date, y = Mean, color = Sim.Type),
            subset(Biomass_comb_graze, Type %in% "Simulated"),
            linewidth = 1) +
  geom_ribbon(aes(x=Date,
                  ymin = pmax((Mean - (SE*2)),0),
                  ymax = pmin((Mean + (SE*2)), 2000),
                  fill = Sim.Type),
              alpha=0.2,
              subset(Biomass_base_graze,
                     Type == "Simulated")) +
  geom_point(aes(x = Date, y = Mean, color = Treatment), 
             subset(Biomass_base_graze, Type == "Observed"), 
             color = "black", size = 1.75, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = Mean - (SE*2), ymax = Mean + (SE*2),
                    color = Treatment),
                subset(Biomass_base_graze, Type %in% "Observed"),
                color = "black", linewidth = 0.7, width = 100)+
  facet_grid(APEXcodeFG ~ Treatment, scales = "free_y") +
  ylab(expression(paste("Ungrazed Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_color_manual(values = cbbPalette,
                     labels = c("Baseline", "Spatial", 
                                "Spatial+Temporal","Temporal"),
                     name = "Simulation") +
  scale_fill_manual(values = "black",
                    name = "Two Standard Error") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

###### ECOLOGICAL SITE #########################################################
bm_cag_combine_plot <- function(direct) {
  
  ## Pasture reference information
  PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
  
  ## Importing observational data
  obv22 <- read.csv("D:/APEX data and scripts/Data/CPER Biomass/AGM_Biomass_Widecln_attr_2023-07-18.csv", header = T)%>%
    select(2:13,17) %>%
    mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
    dplyr::rename(Year = YearSampled) %>%
    select(1:5,9:15) %>%
    subset(Year %in% c(2014:2022)) %>%
    pivot_longer(cols = c(FORB:SS, CSAG:CSPG), names_to = "APEXcodeFG", values_to = "v1") %>%
    group_by(Year,Pasture,Plot,Transect,Treatment,APEXcodeFG) %>%
    summarize(v1 = mean(v1)) %>%
    group_by(Year,Treatment,Pasture,Plot,APEXcodeFG) %>%
    dplyr::summarize(MeankgPerHa = mean(v1), Std = sd(v1)) %>%
    subset(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
             Treatment %in% c("AGM","TGM"))%>%
    mutate(Type = "Observed",
           Date = ymd(paste(Year, 8, 6, sep = "-")),
           WS = NA,
           APEXcodeFG=as.character(APEXcodeFG)) %>%
    ungroup(Year) %>%
    select(-Year)
  
  # Changing plant codes
  obv22$APEXcodeFG <- gsub(replacement = "SSHB", pattern = "SS", obv22$APEXcodeFG)
  obv22$APEXcodeFG <- gsub(replacement = "CSAG", pattern = "VUOC",obv22$APEXcodeFG)
  obv22$APEXcodeFG <- gsub(replacement = "FORB", pattern = "FRB3",obv22$APEXcodeFG)
  obv22$Treatment<-gsub(replacement = "TRM", pattern = "TGM",obv22$Treatment)
  obv22$Treatment<-gsub(replacement = "CARM", pattern = "AGM",obv22$Treatment)
  
  # changing one pasture name
  obv22$Pasture <- gsub(replacement = "10S", pattern = "NH", obv22$Pasture)
  
  ## Adding pasture information
  obv22 <- merge(obv22, PastureID, by = c("Pasture","Plot","Treatment")) %>%
    group_by(Date,Type,Ecosite,APEXcodeFG) %>%
    dplyr::summarise(Mean = mean(MeankgPerHa), 
                     SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)))
  
  setwd(direct)
  
  pathAGM <- "./APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag"
  pathTRM <- "./APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"
  
  # Importing simulated data
  SimBiom_AGM <- fread(pathAGM, header = FALSE, skip = 2, 
                       col.names = name.cag, fill = TRUE) %>%
    filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
    select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           Treatment = "CARM") %>%
    select(-c(Y:D))
  
  SimBiom_TRM <- fread(pathTRM, header = FALSE, skip = 2, 
                       col.names = name.cag, fill = TRUE) %>%
    filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
    select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           Treatment = "TRM") %>%
    select(-c(Y:D))
  
  ## Cleaning reference information
  SimBiom02_AGM <- merge(SimBiom_AGM, PastureID,
                         by = c("ID", "Treatment")) %>%
    mutate(Type = "Simulated",
           Std = 0,
           STL_kgha = STL * 1000,
           STD_kgha = STD * 1000,
           Biomass = STL_kgha + STD_kgha) %>%
    dplyr::rename(APEXcodeFG = CPNM,
                  MeankgPerHa = Biomass) %>%
    select(-c(STL:STD), -c(STL_kgha:STD_kgha))
  
  SimBiom02_TRM <- merge(SimBiom_TRM, PastureID,
                         by = c("ID", "Treatment")) %>%
    mutate(Type = "Simulated",
           Std = 0,
           STL_kgha = STL * 1000,
           STD_kgha = STD * 1000,
           Biomass = STL_kgha + STD_kgha) %>%
    dplyr::rename(APEXcodeFG = CPNM,
                  MeankgPerHa = Biomass) %>%
    select(-c(STL:STD), -c(STL_kgha:STD_kgha))
  
  Biomass_ecosite <- rbind(SimBiom02_AGM, SimBiom02_TRM) %>%
    group_by(Date,Ecosite,Type, APEXcodeFG) %>%
    dplyr::summarise(Mean = mean(MeankgPerHa), 
                     SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa))) %>%
    rbind(obv22)
  
  
  Biomass_ecosite$APEXcodeFG <- gsub("FRB3","FORB",Biomass_ecosite$APEXcodeFG)
  Biomass_ecosite$APEXcodeFG <- gsub("VUOC","CSAG",Biomass_ecosite$APEXcodeFG)
  
  # Biomass_ecosite <- Biomass_ecosite %>% filter(APEXcodeFG %in% c("CSPG", "WSPG"))
  
  return(Biomass_ecosite)
}

Biomass_var_ecosite <- bm_cag_combine_plot(direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div") %>% 
  mutate(Sim.Type = "Spatial")

Biomass_var_pop_ecosite <- bm_cag_combine_plot(direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop") %>% 
  mutate(Sim.Type = "Spatial+Temporal") 

Biomass_comb_ecosite <- rbind(Biomass_var_ecosite, Biomass_var_pop_ecosite) %>%
  filter(!(APEXcodeFG %in% c("CSAG","SSHB", "FORB")))

## Simulation stats
ecosite_compare <- Biomass_comb_ecosite %>% 
  select(-SE) %>%
  filter(month(Date) == 8, day(Date) == 6) %>%
  pivot_wider(names_from = Type, values_from = Mean) 

ecosite_simStats <- ecosite_compare %>%
  group_by(Ecosite, Sim.Type, APEXcodeFG) %>%
  summarise(rmse = round(rmse(Simulated, Observed), 2),
            nrmse = nrmse(Simulated, Observed, norm = "maxmin")/100,
            d = round(d(Simulated, Observed), 3),
            pbias = pbias(Simulated, Observed))

# write.csv(ecosite_simStats, "D:/APEX data and scripts/APEX outputs/ecosite_simStats_04162024.csv")

ggplot() +
  geom_line(aes(x = Date, y = Mean, color = Sim.Type),
            subset(Biomass_comb_ecosite, Type %in% "Simulated"),
            linewidth = 1) +
  geom_point(aes(x = Date, y = Mean, color = Treatment),
             subset(Biomass_comb_ecosite, Type == "Observed"),
             color = "black", size = 1.75, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = Mean - (SE*2), ymax = Mean + (SE*2),
                    color = Treatment),
                subset(Biomass_comb_ecosite, Type %in% "Observed"),
                color = "black", linewidth = 0.7, width = 100) +
  geom_ribbon(aes(x=Date,
                  ymin = pmax((Mean - (SE*2)),0),
                  ymax = pmin((Mean + (SE*2)), 2000),
                  fill = Sim.Type),
              alpha=0.2,
              subset(Biomass_comb_ecosite, Type %in% "Simulated")) +
  facet_grid(APEXcodeFG ~ Ecosite, scales = "free_y")+
  ylab(expression(paste("Ungrazed Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     labels = c("Spatial",
                                "Spatial+Temporal"),
                     name = "Simulation") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                    labels = c("Spatial",
                               "Spatial+Temporal"),
                    name = "Two Standard Error") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))
