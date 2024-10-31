# Code written by Dr. Sean Di Stefano in January 2024
# Code summarizes and visualizes average daily weight gain data from APEX v1605

setwd("C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div")

library(tidyverse)

##### IMPORTING DATA ###########################################################
# Names for columns in DGZ file
name.dgz <- c("ISA", "ID", "HERD","IDOM", "Y", "M", "D", "WSAha", "GNAM", "HRDSZhd",
              "RSTKha/hd","GZWTkg", "TEMPc", "H2ODl/hd", "FORG", "STLt/ha",
              "HUSC", "NCNLg/g", "STDt/ha","NCNDg/g", "DMDkg/ha", "FVI", "TDN",
              "SPLPkg/ha", "SPLDkg/ha", "SPLUkg/ha", "SPDPkg/ha", "SPDDkg/ha",
              "SPDUkg/ha", "HAYkg/ha", "GZSLkg/ha", "GZSDkg/ha", "DTDN",
              "DCNCg/g", "FECEkg/hd", "FECNkg/hd", "FECPkg/hd", "URINkg/hd",
              "URNNkg/hd", "URNPkg/hd", "H20Fkg/hd", "H2OCkg/hd", "MILKkg/hd",
              "GZNBhd","CALVEShd", "CAWTkg", "CAGZkg", "CH4g","DWT", "POP")

## Importing file and setting column names
# Traditionally grazed pastures
tgm_dgz <- read.delim("./Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.DGZ", skip = 10, 
                      header = FALSE, sep = "", col.names = name.dgz) %>%
  mutate(Date = ymd(paste(Y,M,D, sep = "-"))) # adding date column

# Rotationally (adaptively) grazed pastures 
agm_dgz <- read.delim("./Wt Gain Simulation/APEX1605_CO_AGM/CONUNN_TGM.DGZ", skip = 10, 
                      header = FALSE, sep = "", col.names = name.dgz) %>%
  mutate(Date = ymd(paste(Y,M,D, sep = "-")))

## Daily precipitation data from Nicole Kaplan
cper_ppt <- read.csv("C:/APEX data and scripts/Data/CPER PPT/CPER daily climate data_model input.csv") %>%
  filter(year >= 2014) %>% # filtering for first year of CARM experiment
  mutate(date = ymd(paste(year,month,day, sep = "-"))) %>%
  group_by(year, date) %>%
  summarize(RainTotal_mm = sum(rain.mm.))


##### CLEANING DATA ############################################################
# Filter and prepare data for years and pastures of interest
tgm_dgz_filtered <- tgm_dgz %>%
  filter(#ISA %in% c(2:4) & 
    M %in% c(5:9) & Y %in% c(2014:2023)) %>%
  group_by(Date,Y,HERD) %>%
  summarize(DWG = mean(DWT), GZWT = mean(GZWTkg)) 

agm_dgz_filtered <- agm_dgz %>%
  filter(#ISA %in% c(3:6) &
    M %in% c(5:9) & Y %in% c(2014:2023)) %>%
  group_by(Date,Y, HERD, ISA) %>%
  summarize(DWG = mean(DWT), GZWT = mean(GZWTkg)) %>%
  mutate(herd.isa.month = paste(HERD, ISA, month(Date), sep = "-"))

##### DATA VISUALIZATION #######################################################

## Creating scaled data for having two y axes
max.apex <- max(tgm_dgz_filtered$GZWT)

max.ppt <- max(cper_ppt$RainTotal_mm)

# Coefficient for scaling data
coeff <- max.apex/max.ppt

# Scaling precipitation data
ppt <- cper_ppt %>% 
  mutate(ppt_scale = RainTotal_mm*coeff) %>% 
  filter(month(date) %in% c(6:9) & year(date) %in% c(2014:2023)) %>%
  rename(Y = year)

## Visualization of each grazing treatment
ggplot(tgm_dgz_filtered, aes(x = Date, y = DWG)) +
  geom_line(aes(color = as.factor(HERD))) +
  scale_color_discrete(name = "Herd") +
  theme_bw() +
  # geom_bar(data = ppt, aes(x = date, y = ppt_scale,
  #                          fill = "Daily Precip"),
  #          stat = 'identity') +
  scale_fill_manual(name = "Data Type", values = "steelblue") +
  scale_y_continuous("Daily Weight Gain (kg/hd/day)",
                     # limits = c(0,30),
                     sec.axis = sec_axis(~./coeff, name = "Daily Precipitation (mm)")) +
  facet_wrap(~Y, scales = "free_x") +
  theme(text = element_text(size = 15, family = 'serif')) +
  ggtitle("TRM Daily Weight Gain")
  
ggplot(agm_dgz_filtered, aes(x = Date, y = DWG)) +
  geom_line(aes(color = herd.isa.month, 
                linetype = as.factor(HERD)),
            linewidth = 0.75) +
  # geom_point(aes(color = as.factor(ISA)), size = 1) +
  scale_color_discrete(guide = "none") +
  scale_linetype("Herd") +
  theme_bw() +
  facet_wrap(~Y, scales = "free_x") +
  # geom_bar(data = ppt, aes(x = date, y = ppt_scale, 
  #                          fill = "Daily Precip"), 
  #          stat = 'identity') +
  # scale_fill_manual(name = "Data Type", values = "steelblue") +
  scale_y_continuous("Daily Weight (kg)", 
                     # limits = c(0,30),
                     #sec.axis = sec_axis(~./coeff, name = "Daily Precipitation (mm)")
                     ) +
  ggtitle("CARM Daily Weight") +
  theme(text = element_text(size = 15, family = 'serif'))
  
