library(tidyverse)
library(reshape2)
library(data.table)
library(ggsci)
library(wesanderson)

path <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_all92/CONUNN_TGM.cag"

SimBiom_spatHet <- fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
  filter(Y %in% c(2014:2018),
         CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
  select(ID, Y, M, D, CPNM, STL, STD, WS, TS) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         CPNM = recode(CPNM, "FRB3" = "FORB"),  # Rename FRB3 to FORB
         CPNM = factor(CPNM, levels = c("CSPG", "WSPG", "FORB", "VUOC", "SSHB"))  # Order facets
        )

SimBiom_spatHet_stress <- SimBiom_spatHet %>% 
  group_by(Date, CPNM) %>%
  summarize(WS = mean(WS), TS = mean(TS))

plant.stress <- ggplot(SimBiom_spatHet_stress) +
  geom_line(aes(x = Date, y = WS, color = "Water")) +
  geom_line(aes(x = Date, y = TS, color = "Temperature")) +
  facet_wrap(~CPNM, ncol = 1) +
  theme_bw() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1")) +
  labs(y = "Stress Index (0-1)",
       x = "Date",
       color = "Type of Plant Stress") +
  theme(legend.position = "bottom",
        text = element_text(family = "serif", size = 20)) +
  coord_cartesian(expand = FALSE)

plant.stress

setwd("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 biomass")
ggsave(filename = "cper_plantStress_2014-2018.png", plot = plant.stress, width = 16, height = 12)
