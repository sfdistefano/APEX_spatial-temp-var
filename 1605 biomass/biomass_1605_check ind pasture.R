obv22 <- read.csv("D:/APEX data and scripts/Data/CPER Biomass/AGM_Biomass_Widecln_attr_2023-07-18.csv", header = T)%>%
  dplyr::rename(Year = YearSampled) %>%
  subset(Year %in% c(2014:2022)) %>%
  pivot_longer(cols = c(AG:SS), names_to = "APEXcodeFG", values_to = "v1") 

# Changing plant codes
# obv22$APEXcodeFG <- gsub("SS","SSHB",obv22$APEXcodeFG)
obv22$APEXcodeFG <- gsub("AG","CSAG",obv22$APEXcodeFG)
obv22$APEXcodeFG <- gsub("C3PG","CSPG",obv22$APEXcodeFG)
# obv22$APEXcodeFG <- gsub("FORB","FRB3",obv22$APEXcodeFG)
obv22$APEXcodeFG <- gsub("BOBU","WSPG",obv22$APEXcodeFG)

obv22$Treatment<-gsub("TGM","TRM",obv22$Treatment)
obv22$Treatment<-gsub("AGM","CARM",obv22$Treatment)

# changing one pasture name
# obv22$Pasture <- gsub("NH", "10S", obv22$Pasture)

obv22_plot <- obv22 %>%
  group_by(Year,Pasture,Treatment,APEXcodeFG, Plot) %>%
  dplyr::summarize(MeankgPerHa = mean(v1)) %>% 
  subset(Year %in% c(2014:2022) & 
           APEXcodeFG %in% c("WSPG", "CSPG", "FORB", "CSAG") & 
           Treatment %in% c("CARM","TRM")) %>%
  mutate(Type = "Observed", M = 8, D = 6) %>% 
  mutate(Date = ymd(paste(Year, M, D, sep = "-"))) 


# Adding pasture information
obv22_pasture <- merge(obv22_plot, PastureID, 
                           by = c("Pasture","Treatment"),
                           relationship = "many-to-many") %>%
  group_by(Date, Pasture, Treatment, APEXcodeFG) %>%
  summarize(Biomass = mean(MeankgPerHa), Std=sd(MeankgPerHa, na.rm = TRUE)) %>%
  mutate(Type = "Observed")

obv22_ecosite <- merge(obv22_plot, PastureID_20SA, 
                       by = c("Pasture","Treatment"),
                       relationship = "many-to-many") %>%
  group_by(Date, Ecosite, Treatment, APEXcodeFG) %>%
  summarize(Biomass = mean(MeankgPerHa), Std=sd(MeankgPerHa, na.rm = TRUE)) %>%
  mutate(Type = "Observed")

## clean up simulated data
# baseline 1605
SimBiom03_TRM <- SimBiom02_TRM %>%
  filter(Pasture =="19N") %>%
  group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
  summarize(Biomass = mean(MeankgPerHa), Std = sd(MeankgPerHa, na.rm = TRUE))

Biomass_past <- rbind(obv22_pasture, SimBiom03_TRM) %>% 
  filter(Pasture == "19N")

Biomass_past$APEXcodeFG <- gsub("FRB3","FORB",Biomass_past$APEXcodeFG)
Biomass_past$APEXcodeFG <- gsub("VUOC","CSAG",Biomass_past$APEXcodeFG)

Biomass_past_base <- Biomass_past %>% filter(APEXcodeFG %in% c("CSPG", "FORB", "CSAG", "WSPG"))

# baseline + spatial & temporal variability
SimBiom03_TRM <- SimBiom03_TRM %>%
  filter(Pasture =="19N") %>%
  group_by(Date, Pasture, Treatment, Type, APEXcodeFG) %>%
  summarize(Biomass = mean(Biomass), Std = sd(Biomass, na.rm = TRUE))

Biomass_past <- rbind(obv22_pasture, SimBiom03_TRM) %>% 
  filter(Pasture == "19N")

Biomass_past$APEXcodeFG <- gsub("FRB3","FORB",Biomass_past$APEXcodeFG)
Biomass_past$APEXcodeFG <- gsub("VUOC","CSAG",Biomass_past$APEXcodeFG)

Biomass_past_var <- Biomass_past %>% filter(APEXcodeFG %in% c("CSPG", "FORB", "CSAG", "WSPG"))

## Visualize results
ggplot() +
  geom_line(aes(x = Date, y = Biomass, color = APEXcodeFG), 
            subset(Biomass_past_base,Type %in% "Simulated"),
            linewidth = 1) +
  geom_line(aes(x = Date, y = Biomass, color = APEXcodeFG),
            subset(Biomass_past_var,Type %in% "Simulated"),
            linetype = 2, linewidth = 1) +
  geom_point(aes(x = Date, y = Biomass), color = "black",
             subset(Biomass_past_base, Type %in% "Observed"), size = 1.5, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = Biomass - Std, ymax = Biomass + Std,
                    color = Treatment),color = "black",
                subset(Biomass_past_base, Type %in% "Observed"),
                linewidth = 0.5, width = 50) +
  facet_grid(APEXcodeFG~Pasture) +
  newtheme +
  theme(strip.background = element_blank(), #delete the background of facet
        strip.text.x = element_text(angle = 0, hjust =0, vjust =0), #adjust the title of facet
        legend.position = c(0.9, 1.9)) +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  theme(text = element_text(size = 20, family = 'serif')) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  ylab(expression(paste("Standing Biomass (kg ", ha ^-1,")")))
