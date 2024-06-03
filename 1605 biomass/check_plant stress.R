library(tidyverse)
library(reshape2)
library(data.table)
library(hydroGOF)

name.cag <- c("N", "ID", "Y", "M", "D", "CPNM", "WSAha","Grazed.1", "Grazed.2",
              "Grazed.3","HUI", "LAI", "RD", "RW","BIOM", "STL", "CPHT", "STD", 
              "STDL", "GZSL", "GZSD", "A_DDM", "PRCP", "PET", "AET", "AT", "AE",
              "Q", "AT.SP", "WS", "NS", "TS", "MIN_STRESS", "SURF_LIT", 
              "TOTAL_LIT", "POP", "STL_N", "STD_N")

path <- "D:/01-APEX1605_CO_baseline/APEX1605_CO_all20_cagebm/CONUNN_TGM.cag"

SimBiom_base <- fread(path, header = FALSE, skip = 2, 
                 col.names = name.cag, fill = TRUE) %>%
  filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
  select(ID, Y, M, D, CPNM, STL, STD, WS, TS) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  select(-c(Y:D))

SimBiom_base_VUOC <- SimBiom_base %>% 
  filter(CPNM == "VUOC",
         month(Date) %in% c(5:10)) %>%
  group_by(Date, Treatment) %>%
  summarize(WS = mean(WS), TS = mean(TS))

# ggplot(SimBiom_base_VUOC) +
#   geom_point(aes(x = Date, y = WS, color = "WS")) +
#   geom_point(aes(x = Date, y = TS, color = "TS")) +
#   facet_grid(Treatment~year(Date), scales = "free_x") + 
#   ggtitle("Baseline CSAG")


pathTGM <- "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_TGM_cagebm/CONUNN_TGM.cag"

SimBiom_spatial_TGM <- fread(pathTGM, header = FALSE, skip = 2,
                         col.names = name.cag, fill = TRUE) %>%
  filter(Y %in% c(2014:2022) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
  select(ID, Y, M, D, CPNM, STL, STD, WS, TS) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         Treatment = "CARM") %>%
  select(-c(Y:D))

SimBiom_spatial_TGM_VUOC <- SimBiom_spatial_TGM %>% 
  filter(CPNM == "VUOC",
         month(Date) %in% c(5:10)) %>%
  group_by(Date) %>%
  summarize(WS = mean(WS), TS = mean(TS))

SimBiom_base_VUOC_TGM <- SimBiom_base_VUOC %>% filter(Treatment == "TRM")

ggplot(SimBiom_spatial_TGM_VUOC) +
  geom_point(aes(x = Date, y = WS, color = "WS-Spatial")) +
  geom_point(aes(x = Date, y = TS, color = "TS-Spatial")) +
  geom_point(data = SimBiom_base_VUOC_TGM,
             aes(x = Date, y = WS, color = "WS-Base")) +
  geom_point(data = SimBiom_base_VUOC_TGM,
             aes(x = Date, y = TS, color = "TS-Base")) +
  facet_grid(~year(Date), scales = "free_x") +
  ggtitle("Baseline vs Spatial Var. (constant plant pop)")
