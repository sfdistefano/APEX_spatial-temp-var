setwd("D:/APEX1605_for_CPER")

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(scales)
library(data.table)

## column names
name.agz <- c("SA", "ID", "Year", "Year#", "CPNM", "Owner", "Herd","GNAM", 
              "GZNB", "GZD", "GZSL", "GZSD", "HAY", "HAY02", "WTG")

# name.dgz <- c("ISA", "ID", "IDOM", "Y", "M", "D", "WSAha", "GNAM", "HRDSZhd", 
#               "RSTKha/hd","GZWTkg", "TEMPc", "H2ODl/hd", "FORG", "STLt/ha", 
#               "HUSC", "NCNLg/g", "STDt/ha","NCNDg/g", "DMDkg/ha", "FVI", "TDN", 
#               "SPLPkg/ha", "SPLDkg/ha", "SPLUkg/ha", "SPDPkg/ha", "SPDDkg/ha", 
#               "SPDUkg/ha", "HAYkg/ha", "GZSLkg/ha", "GZSDkg/ha", "DTDN", 
#               "DCNCg/g", "FECEkg/hd", "FECNkg/hd", "FECPkg/hd", "URINkg/hd", 
#               "URNNkg/hd", "URNPkg/hd", "H20Fkg/hd", "H2OCkg/hd", "MILKkg/hd", 
#               "GZNBhd","CALVEShd", "CAWTkg", "CAGZkg", "CH4g","DWT")

# name.gzm <- c("SA", "SAID", "Y", "M", "D", "OPERAT", "HERD", "HERDSZhd", 
#               "RSTKha/hd", "GZWTkg/hd", "AGPMt/ha")

## theme for plotting 
newtheme <- theme(plot.background = element_rect(fill = "white", linewidth = 3),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill = NA, color = "black",
                                              size = .2, linetype = "solid"),
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
PastureID <- read.csv("./Experiment/PastureNumber.csv",header = T)

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
AGZ <- read.table("D:/PEST/CONUNN_AGM.AGZ", header = F, skip = 9) %>%
  select(1:15) 

# setting column names
colnames(AGZ) <- name.agz

# labeling grazing treatment
AGZ$Treatment <- ifelse(AGZ$ID %in% c(1:10), "TRM", "CARM")

# summarizing by year * grazing treatment
Gain_AGz <- subset(AGZ, AGZ$Year %in% c(2014: 2022)) %>% 
  select(Year, Treatment, WTG) %>%
  group_by(Year, Treatment) %>%
  dplyr::summarise(MeanWT = mean(WTG), stdevWT = 0, Type = "Simulated") 

## Combining observation and simulated results
ATGMGain <- rbind(Gain_AGz, Gain) %>%
  subset(Year %in% c(2014:2022)) %>%
  mutate(stdevWT = case_when(Type == "Measured" ~ stdevWT))


##### Plotting Results #########################################################
ggplot(ATGMGain, aes(x = Treatment, y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
                    group = Type), width = 0.3, position = position_dodge(.9),
                size = 0.2) +
  geom_text(aes(label = round(MeanWT,2)), 
            position = position_dodge(width = 0.9), vjust = -0.25,
            fontface = "bold", family = "serif") +
  ylab(expression(paste("Cattle weight gain (kg ", hd ^-1," ", day ^-1,")"))) +
  facet_grid(. ~ Year) +
  xlab("Treatment") + 
  scale_y_continuous(limits = c(0, 1.4), expand = c(0, 0))+
  newtheme +
  theme(text = element_text(size = 15, family = 'serif'))

###### Comparing Across Ecological Sites #######################################
## Observed TRM results
# summarizing for each ecological site for TRM 
Gain_ecosite_TRM_obs <- wtg %>% 
  filter(Treatment == "TRM",
         Year %in% c(2014:2022)) %>%
  select(Year, Ecosite, Treatment, gain) %>%
  group_by(Year, Ecosite, Treatment) %>%
  dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                   stdevWT = sd(gain, na.rm = T),
                   Type = "Measured")

## Simulated TRM results
# summarizing for each year
Gain_ecosite_TRM_sim <- AGZ %>%
  filter(Year %in% c(2014:2022),
         Treatment == "TRM") %>%
  merge(PastureID, by = c("ID", "Treatment")) %>%
  select(Year, Ecosite, Treatment, WTG) %>% 
  group_by(Year, Ecosite, Treatment) %>%
  dplyr::summarise(MeanWT = mean(`WTG`,na.rm = T), 
                   stdevWT = 0, Type = "Simulated") 

## Combining results
Gain_ecosite_TRM <- rbind(Gain_ecosite_TRM_obs, Gain_ecosite_TRM_sim)

## Plotting results
ggplot(Gain_ecosite_TRM, aes(x = Type, y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
                    group = Type), width = 0.3, position = position_dodge(.9),
                size = 0.2) +
  geom_text(aes(label = round(MeanWT,2)), 
            position = position_dodge(width = 0.9), vjust = -0.25,
            fontface = "bold", family = "serif") +
  facet_grid(Ecosite ~ Year) +
  ylab(expression(paste("Cattle weight gain (kg ", hd ^-1," ", day ^-1,")"))) +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 1.5)) +
  newtheme +
  theme(text = element_text(size = 15, family = 'serif'))


