library(tidyverse)
library(reshape2)
library(data.table)
library(ggsci)
library(hydroGOF)

setwd("C:/02-APEX1605_spatialtemp/")


PastureID <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv") %>%
  filter(Treatment == "TRM")
PastureID_20sa <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

## Daily precipitation data from Nicole Kaplan
cper_ppt <- read.csv("C:/APEX data and scripts/Data/CPER PPT/CPER daily climate data_model input.csv") %>%
  filter(year >= 2014) %>% # filtering for first year of CARM experiment
  group_by(year) %>%
  summarize(RainTotal_mm = sum(rain.mm.))

##### Experimental Observed Data ###############################################
wtg <- read.csv("C:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2023_SD.csv") %>%
  mutate(gain = ADG / 2.2046) %>%
  merge(PastureID_20sa,by = 'Pasture', relationship = "many-to-many",
        all.x = TRUE)

# summarizing for each treatment type
Gain <- wtg %>%
  select(Year, Treatment, gain) %>%
  group_by(Year, Treatment) %>%
  dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                   SD = sd(gain),
                   Type = "Measured")

# set column names
names(Gain) <- c("Year","Treatment","MeanWT","SD","Type")

# labeling CARM observations
Gain[is.na(Gain)] <- "CARM"
# extract CARM observations
Gain_AGMO <- subset(Gain, Gain$Treatment == "CARM")
Gain_AGMO$Treatment <- NULL

# extract TRM observations
Gain_TGMO <- subset(Gain, Gain$Treatment == "TGM")
Gain_TGMO$Treatment <- "TRM"

##### Simulated results ########################################################
name.agz <- c("SA", "ID", "Year", "Year_num", "CPNM",
              "GNAM", "HERD", "OWNER","GZNB", "GZD", "GZSL", 
              "GZSD", "HAY", "HAYd", "WTG",
              "WSAha", "PSTLkg/ha", "PSTDkg/ha", "PSTTkg/ha",
              "POP", "PEAK")

agm_agz_base <- read.delim("C:/01-APEX1605_CO_baseline/Wt Gain Simulation/APEX1605_CO_all20/CONUNN_TGM.AGZ", 
                           skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year %in% c(2014:2023), ID %in% c(11:20)) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Simulated: no variability", SD = 0)

agm_agz_var <- read.delim("APEX1605_CO_92 subareas_div/Wt Gain Simulation/APEX1605_CO_AGM/CONUNN_TGM.AGZ", 
                          skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year %in% c(2014:2023)) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Simulated: spatial variability", SD = 0)

agm_dgz_var_pop <- read.delim("APEX1605_CO_92 subareas_div_dyn plant pop/Wt Gain Simulation/APEX1605_CO_AGM/CONUNN_TGM.AGZ",
                              skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year %in% c(2014:2023)) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Simulated: spatial+temporal", SD = 0)

AGMGain <- rbind(Gain_AGMO, agm_agz_base, 
                 # agm_agz_pop, 
                 agm_agz_var, 
                 agm_dgz_var_pop
                 )

##### Visualizing scenario comparisons #########################################
## Creating scaled data for having two y axes
max.apex <- min(AGMGain$MeanWT)

max.ppt <- max(cper_ppt$RainTotal_mm)

# Coefficient for scaling data
coeff <- max.apex/max.ppt

# Scaling precipitation data
ppt <- cper_ppt %>% 
  mutate(ppt_scale = RainTotal_mm*coeff)


## Plot scenario results

# color blind friendly palette
cbbPalette <- c("#000000", "#66c2a5", "#fc8d62", "#8da0cb")

ggplot() +
  geom_line(data = AGMGain, aes(x = Year, y = MeanWT, 
                                color = Type)) +
  geom_point(data = AGMGain, aes(x = Year, y = MeanWT, 
                                 color = Type, shape = Type), 
             size = 5) +
  geom_errorbar(data = Gain_AGMO,
                aes(x = Year, y = MeanWT,
                    ymin = MeanWT - SD, 
                    ymax = MeanWT + SD, 
                    color = Type),
                width = 0.25) +
  # geom_bar(data = ppt, aes(x = year, y = ppt_scale, 
  #                          fill = "Annual Precipitation"),
  #          alpha = 0.3,
  #          stat = 'identity') +
  # scale_fill_manual(name = "Data Type", values = "steelblue") +
  scale_y_continuous("Mean Daily Weight Gain (kg/hd/day)", 
                     limits = c(0,1.3)#,
                     # sec.axis = sec_axis(~./coeff, name = "Annual Precipitation (mm)")
                     ) +
  theme_bw() +
  scale_color_manual(values = cbbPalette,
                     name = "Weight Gain Data") +
  # scale_color_brewer(type = "qual", palette = "Dark2") +
  ggtitle("CARM")+
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 
                                2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_shape_manual(values = c(16,15,17,18,16))  +
  theme(text = element_text(size = 20, family = 'serif')) +
  guides(color = guide_legend(title = "Weight Gain Data"),
         shape = guide_legend(title = "Weight Gain Data"))

##### TRADITIONAL GRAZING ######################################################
tgm_agz_base <- read.delim("C:/01-APEX1605_CO_baseline/Wt Gain Simulation/APEX1605_CO_all20/CONUNN_TGM.AGZ", 
                           skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year %in% c(2014:2023), ID %in% c(1:10)) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Simulated: no variability", SD = 0)

tgm_agz_var <- read.delim("APEX1605_CO_92 subareas_div/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ", 
                          skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year %in% c(2014:2023)) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Simulated: spatial variability", SD = 0)

# tgm_agz_var_pop <- read.delim("APEX1605_CO_92 subareas_div_dyn plant pop/Wt Gain Simulation/APEX1605_CO_TGM/CONUNN_TGM.AGZ", 
#                               skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
#   filter(Year %in% c(2014:2023)) %>%
#   group_by(Year) %>%
#   summarize(MeanWT = mean(WTG)) %>%
#   mutate(Type = "Simulated: spatial+temporal variability", SD = 0)

TGMGain <- rbind(Gain_TGMO, 
                 tgm_agz_base, 
                 tgm_agz_var, 
                 tgm_agz_var_pop
                 )

##### Visualizing scenario comparisons #########################################
## Creating scaled data for having two y axes
max.apex <- min(TGMGain$MeanWT)

max.ppt <- max(cper_ppt$RainTotal_mm)

# Coefficient for scaling data
coeff <- max.apex/max.ppt

# Scaling precipitation data
ppt <- cper_ppt %>% 
  mutate(ppt_scale = RainTotal_mm*coeff)


## Plot scenario results
ggplot() +
  geom_line(data = TGMGain, aes(x = Year, y = MeanWT, 
                                color = Type)) +
  geom_point(data = TGMGain, aes(x = Year, y = MeanWT, 
                                 color = Type, shape = Type), 
             size = 5) +
  geom_errorbar(data = Gain_TGMO,
                aes(x = Year, y = MeanWT,
                    ymin = MeanWT - SD, 
                    ymax = MeanWT + SD, 
                    color = Type),
                width = 0.25) +
  # geom_bar(data = ppt, aes(x = year, y = ppt_scale, 
  #                          fill = "Annual Precipitation"),
  #          alpha = 0.3,
  #          stat = 'identity') +
  scale_color_manual(values = cbbPalette,
                     name = "Weight Gain Data") +
  # scale_fill_manual(name = "Data Type", values = "steelblue") +
  scale_y_continuous("Mean Daily Weight Gain (kg/hd/day)", 
                     limits = c(0,1.3)#,
                     # sec.axis = sec_axis(~./coeff, name = "Annual Precipitation (mm)")
                     ) +
  theme_bw() +
  ggtitle("TRM")+
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 
                                2018, 2019, 2020, 2021, 2023)) +
  scale_shape_manual(values = c(16,15,17,18,16))  +
  theme(text = element_text(size = 20, family = 'serif')) +
  guides(color = guide_legend(title = "Weight Gain Data"),
         shape = guide_legend(title = "Weight Gain Data"))

## Simulation comparison statistics (model performance)
# AGMGain_compare <- AGMGain %>%
#   select(-SD) %>%
#   filter(Type != "Measured") %>%
#   merge(Gain_AGMO, by = "Year") %>%
#   select(-SD, -Type.y) %>%
#   rename(Simulated = MeanWT.x, Observed = MeanWT.y,
#          Sim.Type = Type.x)  %>%
#   mutate(Treatment = "CARM")
# 
# TGMGain_compare <- TGMGain %>%
#   select(-SD, -Treatment) %>%
#   filter(Type != "Measured") %>%
#   merge(Gain_TGMO, by = "Year") %>%
#   select(-SD, -Type.y) %>%
#   rename(Simulated = MeanWT.x, Observed = MeanWT.y,
#          Sim.Type = Type.x) 
# 
# Gain_simStats <- rbind(AGMGain_compare, TGMGain_compare) %>%
#   group_by(Treatment, Sim.Type) %>%
#   summarise(rmse = round(rmse(Simulated, Observed), 2),
#             nrmse = nrmse(Simulated, Observed, norm = "maxmin")/100,
#             d = round(d(Simulated, Observed), 3),
#             pbias = pbias(Simulated, Observed))

###### Plot of baseline scenario vs measured ###################################
tgm_sim <- tgm_agz_base %>% 
  mutate(Type = "Simulated",
         Treatment = "TRM")
agm_sim <- agm_agz_base %>% 
  mutate(Type = "Simulated",
         Treatment = "CARM")

Gain_AGMO <- Gain_AGMO %>%
  mutate(Treatment = "CARM")

wt_gain <- rbind(tgm_sim, agm_sim, Gain_TGMO, Gain_AGMO)


ggplot(wt_gain, aes(x = Treatment, y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD,
                    group = Type), width = 0.3, position = position_dodge(.9),
                linewidth = 0.2) +
  geom_text(aes(label = round(MeanWT,2)),
            position = position_dodge(width = 0.9),
            vjust = -0.25,
            fontface = "bold", family = "serif") +
  ylab(expression(paste("Average Daily Weight Gain (kg ", hd ^-1," ", day ^-1,")"))) +
  facet_grid(. ~ Year) +
  xlab("Treatment") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme(text = element_text(size = 15, family = 'serif')) +
  theme_bw()
