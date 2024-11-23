## Code written by Sean Di Stefano 03/2024, altered from code written by Gong Cheng

library(tidyverse)
library(data.table)
library(hydroGOF)

name.cag <- c("N", "ID", "Y", "M", "D", "CPNM", "WSAha","Grazed.1", "Grazed.2",
              "Grazed.3","HUI", "LAI", "RD", "RW","BIOM", "STL", "CPHT", "STD", 
              "STDL", "GZSL", "GZSD", "A_DDM", "PRCP", "PET", "AET", "AT", "AE",
              "Q", "AT.SP", "WS", "NS", "TS", "MIN_STRESS", "SURF_LIT", 
              "TOTAL_LIT", "POP", "STL_N", "STD_N", "SurfaceN")

###### GRAZING TREATMENT #######################################################
#### IMPORT & CLEAN DATA 
bm_cag_combine <- function(num.sa = 20, direct){
  
  setwd(direct)
  
  if(num.sa == 20){
    
    path <- "./Cage Biomass Simulation/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"
    
    ## Importing simulated data
    SimBiom <- fread(path, header = FALSE, skip = 2, 
                     col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2023) & 
               CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
      select(-c(Y:D))
    
    ## Importing reference information
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
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
    obv22 <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", header = T)%>%
      select(2:13,17) %>%
      mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
      dplyr::rename(Year = YearSampled) %>%
      select(1:5,9:15) %>%
      subset(Year %in% c(2014:2023)) %>%
      pivot_longer(cols = c(FORB:SS, CSAG:CSPG), 
                   names_to = "APEXcodeFG", values_to = "v1") %>%
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
      dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)),
                       SD = sd(MeankgPerHa))
    
  } else if(num.sa == 92) {
    
    pathAGM <- "./Cage Biomass Simulation/APEX1605_CO_AGM_cagebm/CONUNN_TGM.cag"
    pathTRM <- "./Cage Biomass Simulation/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"
    
    # Importing simulated data
    SimBiom_AGM <- fread(pathAGM, header = FALSE, skip = 2,
                         col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2023) & 
               CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = "CARM") %>%
      select(-c(Y:D))
    
    SimBiom_TRM <- fread(pathTRM, header = FALSE, skip = 2,
                         col.names = name.cag, fill = TRUE) %>%
      filter(Y %in% c(2014:2023) & 
               CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
      select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
      mutate(Date = ymd(paste(Y, M, D, sep = "-")),
             Treatment = "TRM") %>%
      select(-c(Y:D))
    
    ## Importing reference information
    PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
    
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
    obv22 <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", header = T)%>%
      select(2:13,17) %>%
      mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
      dplyr::rename(Year = YearSampled) %>%
      select(1:5,9:15) %>%
      subset(Year %in% c(2014:2023)) %>%
      pivot_longer(cols = c(FORB:SS, CSAG:CSPG), 
                   names_to = "APEXcodeFG", values_to = "v1") %>%
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
      dplyr::summarise(SE = sd(MeankgPerHa)/sqrt(length(MeankgPerHa)),
                       SD = sd(MeankgPerHa))
    
  } else{
    print("incompatible number of subareas")
  }
  
  Biomass_pasture <- merge(Biomass_pasture, Biomass_pasture_se)
  
  Biomass_pasture$APEXcodeFG <- gsub("FRB3","FORB",Biomass_pasture$APEXcodeFG)
  Biomass_pasture$APEXcodeFG <- gsub("VUOC","CSAG",Biomass_pasture$APEXcodeFG)
  
  Biomass_pasture <- Biomass_pasture %>% 
    filter(APEXcodeFG %in% c("CSPG", "FORB", "CSAG", "WSPG", "SSHB"))
  
  return(Biomass_pasture)
}

## Baseline
Biomass_base_graze <- bm_cag_combine(num.sa = 20, 
                                     direct = "C:/01-APEX1605_CO_baseline/")  %>% 
  mutate(Sim.Type = "Simulated: no variability") 

## Spatial Variability
Biomass_var_graze <- bm_cag_combine(num.sa = 92,
                                    direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div") %>%
  mutate(Sim.Type = "Simulated: spatial variability")

# ## Spatial + Temporal Variability
Biomass_var_pop_graze <- bm_cag_combine(num.sa = 92,
                                        direct = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop") %>%
  mutate(Sim.Type = "Simulated: spatial+temporal variability")

# ### COMPARE ACROSS SCENARIOS @ GRAZING TREATMENT LEVEL 
Biomass_comb_graze <- rbind(Biomass_base_graze,
                            Biomass_var_graze#,
                            #Biomass_var_pop_graze
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

# write.csv(graze_simStats, "C:/APEX data and scripts/APEX outputs/grazeTrt_simStats_07112024.csv")

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
  geom_errorbar(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD,
                    color = "Observed"),
                subset(Biomass_base_graze, Type %in% "Observed"),
                linewidth = 0.7, width = 100) +
  facet_grid(factor(APEXcodeFG, 
                     levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")) ~ Treatment, 
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

##### NO VARIABILITY SCENARIO ##################################################
## Importing reference information
PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv") %>%
  mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))

## Importing simulated data
path <- "C:/01-APEX1605_CO_baseline/Cage Biomass Simulation/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"

SimBiom <- fread(path, header = FALSE, skip = 2, 
                 col.names = name.cag, fill = TRUE) %>%
  filter(Y %in% c(2014:2023)) %>%
  select(ID, Y, M, D, CPNM, STL, STD, WS) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  select(-c(Y:D))

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
obv22 <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", header = T)%>%
  select(2:13,17) %>%
  mutate(CSAG = AG, WSPG = (BOBU+WSPG)/2, CSPG = C3PG) %>%
  dplyr::rename(Year = YearSampled) %>%
  select(1:5,9:15) %>%
  subset(Year %in% c(2014:2023)) %>%
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
  group_by(Date, Pasture, Treatment, Type) %>%
  dplyr::summarize(Total_kgPerHa = sum(MeankgPerHa))

Biomass_trt <- Biomass_pasture %>%
  group_by(Date,Treatment,Type) %>%
  dplyr::summarise(Total_Mean = mean(Total_kgPerHa))

Biomass_pasture_se <- Biomass_pasture %>%
  group_by(Date, Treatment, Type) %>%
  dplyr::summarise(SE = sd(Total_kgPerHa)/sqrt(length(Total_kgPerHa)),
                   SD = sd(Total_kgPerHa))

Biomass_trt_grazeTotal <- merge(Biomass_trt, Biomass_pasture_se,
                            by = c("Date", "Treatment", "Type"))

## Simulated Plant Stress Data
path <- "C:/01-APEX1605_CO_baseline/Cage Biomass Simulation/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"

## Importing simulated data
SimBiom_plantStress <- fread(path, header = FALSE, skip = 2, 
                             col.names = name.cag, fill = TRUE) %>%
  filter(Y %in% c(2014:2023) & 
           CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
  select(ID, Y, M, D, CPNM, WS, TS) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  select(-c(Y:D)) %>%
  group_by(Date, CPNM) %>%
  summarize(WS_mean = mean(WS), TS_mean = mean(TS),
            WS_SD = sd(WS), TS_SD = sd(TS))

SimBiom_plantStress$CPNM <- gsub(pattern = "VUOC", replacement = "CSAG",
                                 x = SimBiom_plantStress$CPNM)
SimBiom_plantStress$CPNM <- gsub(pattern = "FRB3", replacement = "FORB",
                                 x = SimBiom_plantStress$CPNM)
