# Code written Dr. Sean Di Stefano
# Code summarizes and visualizes daily biomass outputs for different plant functional group
# Written for data format that replicates clipped biomass collected from the field
# Simulated data has a folder for each year where grazing is not applied

setwd("D:/APEX1605_for_CPER/")

library(plyr)
library(data.table)
library(tidyverse)
library(reshape2)

# Visualization theme for later ggplots
newtheme <- theme(plot.background = element_rect(fill = "white", linewidth=3),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill=NA,color="black", 
                                              linewidth=.5, linetype="solid"),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  legend.position = c(0.1, 0.95),
                  plot.title = element_text(hjust = 0.5, vjust = 0),
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 30, 
                                             hjust = 0.5, vjust = 0.5),
                  axis.line = element_line(colour="black"),
                  text = element_text(size = 10),
                  legend.background = element_blank(),
                  panel.spacing = unit(0.2, "lines"),
                  legend.title = element_text(size = 10,face="bold"),
                  legend.key = element_blank())

cb_palette <- c("#0072B2","#000000","#56B4E9","#009E73","#F0E442","#D55E00","#E69F00","#CC79A7")

#### REFERENCE INFORMATION #####################################################
PastureID <- read.csv("./Experiment/PastureNumber.csv",header = T)

#### SIMULATED DATA ############################################################
# Column names for SAD file (biomass output)
name.sad <- c("N", "ID", "Y", "M", "D", "CPNM", "HUI", "LAI", "RD", "RW", 
              "BIOM", "STL", "CPHT", "STD", "STDL", "WS", "NS", "PS", "TS", 
              "AS", "SALT", "REG", "DM1", "L2D", "R2S", "RFRC", "DSTL", "Q", 
              "PRK", "EI", "C", "USLE", "MUSL", "REMX", "RUS2", "ZNH3", "ZNO3",
              "ZPML", "UNM", "UPM", "RZSW", "WTBL", "GWST", "XXXX", "RSD", 
              "RSVQ", "RSVY", "RSSA", "SWLT", "SNO", "RSDM", "GWSN", "AGPM")
## Concatenate data from multiple folders into one file
# CARM (AGM) simulated data
res <- NULL 
SimAGM <- data.frame() 

for (i in 14:22){
  path <- paste0(getwd(),'/','20',i,'/APEX1605_CO_AGM/CONUNN_AGM.SAD')
  res <- c(res,path)
  year <- paste0('20', i)
  
  SimB <- fread(path, header = FALSE, skip = 10, col.names = name.sad) %>%
    subset(Y %in% year & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB"), 
           select = c(ID, Y, M, D, CPNM, STL, WS, TS)) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           Treatment = "CARM") %>%
    select(-c(Y:D))
  
  SimAGM <- rbind(SimAGM, SimB)
}

# TRM simulated data
res <- NULL
SimTGM <- data.frame()

for (i in 14:22){
  path <- paste0(getwd(),'/','20',i,'/APEX1605_CO_TGM/CONUNN_TGM.SAD')
  res <- c(res,path)
  year <- paste0('20', i)
  
  SimB <- fread(path, header = FALSE, skip = 10, col.names = name.sad) %>%
    subset(Y %in% year & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB"), 
           select = c(ID, Y, M, D, CPNM, STL, WS, TS)) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           Treatment = "TRM") %>%
    select(-c(Y:D))
  
  SimTGM <- rbind(SimTGM, SimB)
}

## Combining simulated outcomes for both grazing treatments
SimBiom <- rbind(SimAGM,SimTGM)

# Adding pasture information
SimBiom <- left_join(SimBiom,PastureID, by = c("ID", "Treatment")) %>%
  mutate(Type = "Simulated", StdkgPerHa = 0,
         STL = STL * 1000)

# Setting column names for new dataframe
names(SimBiom) <- c('ID', 'APEXcodeFG', 'MeankgPerHa', 'water_stress',
                    'temp_stress', 'Date','Treatment', 'Pasture', 'Ecosite',
                    'Type', 'StdkgPerHa')

###### Experimental Observations ###############################################
obv22 <- read.csv("D:/APEX data and scripts/Data/CPER Biomass/AGM_Biomass_Widecln_attr_2023-07-18.csv", header = T)%>%
  select(2:13,17) %>%
  mutate(TOTAL = AG + WSPG + C3PG + SS + SD + BOBU + FORB,
         CSAG = AG, 
         WSPG = BOBU + WSPG, # combined when BOBU is included as WSPG in APEX plant groups 
         CSPG = C3PG) %>%
  dplyr::rename(Year = YearSampled) %>%
  select(1:5,9:16) %>%
  subset(Year %in% c(2013:2022)) %>%
  pivot_longer(cols = c(FORB:SS, TOTAL:CSPG), 
               names_to = "APEXcodeFG", 
               values_to = "v1") %>%
  group_by(Year,Pasture,Treatment,APEXcodeFG) %>%
  dplyr::summarize(MeankgPerHa = mean(v1), StdkgPerHa = sd(v1)) %>% 
  subset(Year %in% c(2014:2022) & 
           APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") & 
           Treatment %in% c("AGM","TGM"))%>%
  mutate(Type = "Observed", M = 8, D = 6, 
         water_stress = 1, temp_stress = 1,
         APEXcodeFG=as.character(APEXcodeFG)) %>% 
  mutate(Date = ymd(paste(Year, M, D, sep = "-"))) 

# Changing plant codes
obv22$APEXcodeFG <- gsub("SS","SSHB",obv22$APEXcodeFG)
obv22$APEXcodeFG <- gsub("CSAG","VUOC",obv22$APEXcodeFG)
obv22$APEXcodeFG <- gsub("FORB","FRB3",obv22$APEXcodeFG)
obv22$Treatment<-gsub("TGM","TRM",obv22$Treatment)
obv22$Treatment<-gsub("AGM","CARM",obv22$Treatment)

# Adding pasture information
obv22 <- left_join(obv22, PastureID, by = c("Pasture","Treatment"))

##### Plotting Comparisons #####################################################

# grazing treatment * functional group
Biomass <- rbind(obv22,SimBiom) %>%
  mutate(Mean = MeankgPerHa, Std = StdkgPerHa)%>%
  group_by(Date,Treatment,Type, APEXcodeFG)%>%
  dplyr::summarise(Mean = mean(MeankgPerHa), Std=sd(MeankgPerHa))

ggplot()+
  geom_line(aes(x = Date, y = Mean, color = Treatment),
            subset(Biomass,Type %in% "Simulated"), 
            linewidth=0.25, color="red")+
  geom_ribbon(aes(x=Date, ymin = Mean - Std, ymax = Mean + Std, 
                  fill = Treatment),
              subset(Biomass,Type %in% "Simulated"),alpha=0.2)+
  geom_point(aes(x = Date, y = Mean, color = Treatment), color = "black",
             subset(Biomass, Type %in% "Observed"), size = 1, stroke = 0.7)+
  geom_errorbar(aes(x = Date, ymin = Mean - Std, ymax = Mean + Std, 
                    color = Treatment),color = "black",
                subset(Biomass, Type %in% "Observed"), 
                linewidth = 0.25, width = 20)+
  facet_grid(APEXcodeFG ~ Treatment)+
  ylab(expression(paste("Functional group biomass (kg ", ha ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_y_continuous(limits = c(-50, 1500),breaks = c(seq(0, 1000 ,500)),
                     expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_shape_manual(values=c(17,21)) + # 
  newtheme+
  theme(strip.background = element_blank(), #delete the background of facet
        strip.text.x = element_text(angle = 0, hjust =0, vjust =-1.5), #adjust the title of facet
        legend.position = c(0.9, 1.9)) +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  theme(text = element_text(size = 15, family = 'serif'))

# ecological site * functional group
Biomass <- rbind(obv22,SimBiom) %>%
  mutate(Mean = MeankgPerHa,Std = StdkgPerHa)%>%
  group_by(Date, Ecosite, Type, APEXcodeFG)%>%
  dplyr::summarise(Mean = mean(MeankgPerHa), Std = sd(MeankgPerHa))

ggplot()+
  geom_line(aes(x = Date, y = Mean, color = Treatment),
            subset(Biomass,Type %in% "Simulated"), 
            linewidth = 0.25, color="red")+
  geom_ribbon(aes(x=Date, ymin = Mean - Std, ymax = Mean + Std, 
                  fill = Ecosite),
              subset(Biomass,Type %in% "Simulated"),alpha=0.2)+
  geom_point(aes(x = Date, y = Mean, color = Treatment), color = "black",
             subset(Biomass, Type %in% "Observed"), size = 1, stroke = 0.7)+
  geom_errorbar(aes(x = Date, ymin = Mean - Std, ymax = Mean + Std, 
                    color = Treatment),color = "black",
                subset(Biomass, Type %in% "Observed"), 
                linewidth = 0.5, width = 50)+
  facet_grid(APEXcodeFG ~ Ecosite)+
  ylab(expression(paste("Functional group biomass (kg ", ha ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_y_continuous(limits = c(-50, 2000),
                     # breaks = c(seq(0, 1000 , 500, 1500)),
                     expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_shape_manual(values=c(17,21)) + # 
  newtheme+
  theme(strip.background = element_blank(), #delete the background of facet
        strip.text.x = element_text(angle = 0, hjust =0, vjust =-1.5), #adjust the title of facet
        legend.position = c(0.9, 1.9)) +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  theme(text = element_text(size = 15, family = 'serif'))
