# Code written by Dr. Sean Di Stefano in January 2024
# Code summarizes and visualizes annual weight gain data from APEX v1605
# This version of APEX 1605 was altered by Dr. Liwang Ma USDA-ARS

setwd("C:/APEX1605_CO_woLoop")

library(tidyverse)
library(reshape2)
library(ggthemes)
library(scales)
library(data.table)

## column names
name.agz <- c("SA", "ID", "Year", "Year_num", "CPNM",
              "GNAM", "HERD", "OWNER","GZNB", "GZD", "GZSL", 
              "GZSD", "HAY", "HAYd", "WTG",
              "WSAha", "PSTLkg/ha", "PSTDkg/ha", "PSTTkg/ha",
              "POP", "PEAK")

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

## Pasture reference information
PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

##### Experimental Observed Data ###############################################
wtg <- fread("D:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2022.csv") %>%
  mutate(gain = ADG / 2.2046) %>%
  left_join(PastureID,by = 'Pasture')

# summarizing for each treatment type
Gain <- wtg %>% 
  select(Year, Treatment, gain) %>% 
  group_by(Year, Treatment) %>%
  dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                   stdevWT = sd(gain, na.rm = T),
                   Type = "Measured")

# set column names
names(Gain) <- c("Year","Treatment","MeanWT","stdevWT","Type")

# labeling CARM observations
Gain[is.na(Gain)] <- "CARM"
# extract CARM observations
Gain_AGMO <- subset(Gain, Gain$Treatment == "CARM")
Gain_AGMO$Treatment <- NULL

# extract TRM observations
Gain_TGMO <- subset(Gain, Gain$Treatment == "TRM")
Gain_TGMO$Treatment <- NULL

##### APEX Simulated Data ######################################################
## Simulated CARM results 
AGM_AGZ <- read.table("./APEX1605_CO_AGM/CONUNN_AGM.AGZ", header = F,
                      skip = 9, col.names = name.agz)

# summarizing for each year
Gain_AGz <- subset(AGM_AGZ, AGM_AGZ$Year %in% c(2014: 2022)) %>% 
  select(Year, WTG) %>%
  group_by(Year) %>%
  dplyr::summarise(MeanWT = mean(WTG), stdevWT = 0, Type = "Simulated") 

# combining with experimental observed data
AGMGain<- rbind(Gain_AGz, Gain_AGMO)
AGMGain$Treatment <- "CARM"

## Simulated TRM results
TGM_AGZ <- read.table("./APEX1605_CO_TGM/CONUNN_TGM.AGZ", header = F,
                      skip = 9, col.names = name.agz)

# summarizing for each year
Gain_AGz <- subset(TGM_AGZ, TGM_AGZ$Year %in% c(2014: 2022)) %>%
  select(Year, `WTG`) %>% 
  group_by(Year) %>%
  dplyr::summarise(MeanWT = mean(`WTG`,na.rm = T), stdevWT = 0, Type = "Simulated")

# combining with experimental observed data
TGMGain<- rbind(Gain_AGz, Gain_TGMO)
TGMGain$Treatment <- "TRM"

# combine CARM and TRM data (observational + simulated)
ATGMGain <- rbind(AGMGain, TGMGain) %>%
  subset(Year %in% c(2014:2022)) %>% # years of experiment
  mutate(stdevWT = case_when(Type == "Measured" ~ stdevWT))


##### Plotting Results #########################################################

# grazing treatment * year
ggplot(ATGMGain, aes(x = Treatment, y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
                    group = Type), width = 0.3, position = position_dodge(.9),
                linewidth = 0.2) +
  geom_text(aes(label = round(MeanWT,2)),
            position = position_dodge(width = 0.9), vjust = -0.25,
            fontface = "bold", family = "serif") +
  ylab(expression(paste("Cattle weight gain (kg ", hd ^-1," ", day ^-1,")"))) +
  facet_grid(. ~ Year) +
  xlab("Treatment") +
  ggtitle(" ") +  #CARM & TRM Seasonal Animal Daily Gain
  scale_y_continuous(limits = c(0, 1.4), expand = c(0, 0))+
  newtheme +
  theme(text = element_text(size = 15, family = 'serif'))

###### COMPARING ACROSS ECOLOGICAL SITES #######################################
## Observed TRM results
# summarizing for each ecological site for TRM 
# Gain_ecosite_TRM_obs <- wtg %>% 
#   filter(Treatment == "TRM",
#          Year %in% c(2014:2022)) %>%
#   select(Year, Ecosite, Treatment, gain) %>%
#   group_by(Year, Ecosite, Treatment) %>%
#   dplyr::summarise(MeanWT = mean(gain, na.rm = T),
#                    stdevWT = sd(gain, na.rm = T),
#                    Type = "Measured")
# 
# ## Simulated TRM results
# # summarizing for each year
# Gain_ecosite_TRM_sim <- TGM_AGZ %>%
#   filter(Year %in% c(2014:2022)) %>%
#   merge(PastureID, by.x = "ID", by.y = "PastureID") %>%
#   select(Year, Ecosite, Treatment, WTG) %>% 
#   group_by(Year, Ecosite, Treatment) %>%
#   dplyr::summarise(MeanWT = mean(`WTG`,na.rm = T), 
#                    stdevWT = 0, Type = "Simulated") 
# 
# ## Combining results
# Gain_ecosite_TRM <- rbind(Gain_ecosite_TRM_obs, Gain_ecosite_TRM_sim)
# 
# ## Plotting results
# ggplot(Gain_ecosite_TRM, aes(x = "   ", y = MeanWT, fill = Type)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
#                     group = Type), width = 0.3, position = position_dodge(.9),
#                 linewidth = 0.2) +
#   geom_text(aes(label = round(MeanWT,2)),
#             position = position_dodge(width = 0.9), vjust = -0.25,
#             fontface = "bold", family = "serif") +
#   facet_grid(Ecosite ~ Year) +
#   ylab(expression(paste("Cattle weight gain (kg ", hd ^-1," ", day ^-1,")"))) +
#   xlab("Year") +
#   scale_y_continuous(limits = c(0, 1.5)) +
#   newtheme +
#   theme(text = element_text(size = 15, family = 'serif'))
