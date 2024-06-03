# Code written by Dr. Sean Di Stefano in January 2024
# Code summarizes and visualizes annual weight gain data from APEX v1605
# This version of APEX 1605 was altered by Dr. Liwang Ma USDA-ARS

setwd("D:/01-APEX1605_CO_baseline/APEX1605_CO_all20")

library(tidyverse)
library(reshape2)
library(ggthemes)
library(scales)
library(data.table)

PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

## theme for plotting 
newtheme <- theme(plot.background = element_rect(fill = "white", linewidth = 3),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill = NA, color = "black",
                                              linewidth = .2, linetype = "solid"),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5, vjust = 0),
                  axis.text.y = element_text(size = 10), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "top",
                  legend.background = element_blank(),        
                  panel.spacing = unit(0.2, "lines"), 
                  legend.title = element_text(size = 10,face = "bold"),
                  legend.key = element_blank())
## column names
name.agz <- c("SA", "ID", "Year", "Year_num", "CPNM",
              "GNAM", "HERD", "OWNER","GZNB", "GZD", "GZSL", 
              "GZSD", "HAY", "HAYd", "WTG",
              "WSAha", "PSTLkg/ha", "PSTDkg/ha", "PSTTkg/ha",
              "POP", "PEAK")

##### Experimental Observed Data ###############################################
wtg <- fread("D:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2022.csv") %>%
  mutate(gain = ADG / 2.2046) %>%
  merge(PastureID, by = 'Pasture',
        all.x = TRUE)


##### IMPORTING DATA ###########################################################
ADWG_agz_combine <- function(num.sa = 20, direct) {
  
  setwd(direct)
  
  # summarizing for each treatment type
  gain <- wtg %>% 
    select(Year, Treatment, gain) %>% 
    group_by(Year, Treatment) %>%
    dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                     stdevWT = sd(gain, na.rm = T)) %>%
    mutate(Type = "Measured",
           Treatment = ifelse(Treatment == "TGM", "TRM", "CARM"))
  
  # labeling CARM observations
  gain[is.na(gain)] <- "CARM"
  
  if(num.sa == 20) {
    
    path <- "./APEX1605_CO_all20/CONUNN_TGM.agz"
    
    ## Simulated TRM results
    agz <- read.table(path, header = F,
                      skip = 9, col.names = name.agz)
    
    # summarizing for each year
    gain_agz <- agz %>% 
      filter(Year >= 2014) %>%
      select(ID, Year, WTG) %>%
      mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
      group_by(Year, Treatment) %>%
      dplyr::summarise(MeanWT = mean(WTG,na.rm = T), stdevWT = 0) %>% 
      mutate(Type = "Simulated")
    
    # combining with experimental observed data
    gain_comb <- rbind(gain, gain_agz) %>%
      subset(Year %in% c(2014:2022)) %>% # years of experiment
      mutate(stdevWT = case_when(Type == "Measured" ~ stdevWT))
    
  } else if(num.sa == 92) {
    
    pathAGM <- "./APEX1605_CO_AGM/CONUNN_TGM.agz"
    pathTRM <- "./APEX1605_CO_TGM/CONUNN_TGM.agz"
    
    ## Simulated CARM results
    agm_agz <- read.table(pathAGM, header = F,
                          skip = 9, col.names = name.agz)
    
    # summarizing for each year
    gain_agz_agm <- agm_agz %>%
      filter(Year >= 2014) %>%
      select(Year, WTG) %>%
      group_by(Year) %>%
      dplyr::summarise(MeanWT = mean(WTG), stdevWT = 0) %>%
      mutate(Type = "Simulated",
             Treatment = "CARM")
    
    ## Simulated TRM results
    tgm_agz <- read.table(pathTRM, header = F,
                          skip = 9, col.names = name.agz)
    
    # summarizing for each year
    gain_agz_tgm <- tgm_agz %>%
      filter(Year >= 2014) %>%
      group_by(Year) %>%
      dplyr::summarise(MeanWT = mean(`WTG`,na.rm = T), stdevWT = 0) %>%
      mutate(Type = "Simulated",
             Treatment = "TRM")
    
    # combine CARM and TRM data (observational + simulated)
    gain_comb <- rbind(gain, gain_agz_agm, gain_agz_tgm) %>%
      filter(Year >= 2014) %>% # years of experiment
      mutate(stdevWT = case_when(Type == "Measured" ~ stdevWT))
    
  } else{
    print("incompatible number of subareas")
  }
  
  ### Visualize data
  plot <- ggplot(gain_comb, aes(x = Treatment, y = MeanWT, fill = Type)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
                      group = Type), width = 0.3, position = position_dodge(.9),
                  linewidth = 0.2) +
    geom_text(aes(label = round(MeanWT,2)),
              position = position_dodge(width = 0.9), vjust = -0.25,
              fontface = "bold", family = "serif") +
    ylab(expression(paste("Average Daily Weight Gain (kg ", hd ^-1," ", day ^-1,")"))) +
    facet_grid(. ~ Year) +
    xlab("Treatment") +
    scale_y_continuous(limits = c(0, 1.4), expand = c(0, 0))+
    newtheme +
    theme(text = element_text(size = 15, family = 'serif'))
  
  return(plot)
}

ADWG_agz_combine(num.sa = 20, direct = "D:/01-APEX1605_CO_baseline/")
ADWG_agz_combine(num.sa = 92, direct = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div_dyn plant pop")

###### COMPARING ACROSS ECOLOGICAL SITES #######################################
## Observed TRM results
# summarizing for each ecological site for TRM 
Gain_ecosite_TRM_obs <- wtg %>% 
  # mutate(Treatment = ifelse(Treatment == "TGM", "TRM", "CARM")) %>%
  filter(Treatment == "TGM",
         Year %in% c(2014:2022)) %>%
  select(Year, Ecosite, Treatment, gain) %>%
  group_by(Year, Ecosite, Treatment) %>%
  dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                   stdevWT = sd(gain, na.rm = T),
                   Type = "Measured")

## Simulated TRM results
# summarizing for each year
pathTRM <- "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_TGM/CONUNN_TGM.agz"

tgm_agz <- read.table(pathTRM, header = F,
                      skip = 9, col.names = name.agz)

Gain_ecosite_TRM_sim <- tgm_agz %>%
  filter(Year %in% c(2014:2022)) %>%
  merge(PastureID, by = "ID") %>%
  select(Year, Ecosite, Treatment, WTG) %>% 
  group_by(Year, Ecosite, Treatment) %>%
  dplyr::summarise(MeanWT = mean(`WTG`,na.rm = T), 
                   stdevWT = 0, Type = "Simulated") 

## Combining results
Gain_ecosite_TRM <- rbind(Gain_ecosite_TRM_obs, Gain_ecosite_TRM_sim) %>%
  filter(Treatment == "TGM")

## Plotting results
ggplot(Gain_ecosite_TRM, aes(x = "   ", y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
                    group = Type), width = 0.3, position = position_dodge(.9),
                linewidth = 0.2) +
  geom_text(aes(label = round(MeanWT,2)),
            position = position_dodge(width = 0.9), vjust = -0.25,
            fontface = "bold", family = "serif") +
  facet_grid(Ecosite ~ Year) +
  ylab(expression(paste("Cattle weight gain (kg ", hd ^-1," ", day ^-1,")"))) +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 1.5)) +
  newtheme +
  theme(text = element_text(size = 15, family = 'serif'))
