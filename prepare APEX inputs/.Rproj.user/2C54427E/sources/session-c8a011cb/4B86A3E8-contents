library(tidyverse)
library(reshape2)
library(data.table)
library(hydroGOF)

OPC_col.names <-  c("ID","Year_num", "Month", "Day", "Till", "Tractor",
                    "apex.code", "JX7", "HERD", "OPV1", "OPV2", "OPV3", "apex.input",
                    "OPV6", "OPV7")

OPC.files_sa20 <- list.files(path = "D:/01-APEX1605_CO_baseline/APEX1605_CO_all20",
                             pattern = "\\.OPC$")

OPC.files_sa92_AGM <- list.files(path = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_AGM",
                                 pattern = "\\.OPC$")

OPC.files_sa92_TGM <- list.files(path = "D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_TGM",
                                 pattern = "\\.OPC$")


setwd("D:/01-APEX1605_CO_baseline/APEX1605_CO_all20")
OPC.input_sa20 <- lapply(OPC.files_sa20, function(file) {
  fread(file, skip = 2, nrows = 6,
        sep = " ", fill = TRUE)
})

setwd("D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_AGM")
OPC.input_sa92_AGM <- lapply(OPC.files_sa92_AGM, function(file) {
  fread(file, skip = 2, nrows = 6,
        sep = " ", fill = TRUE)
})

setwd("D:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/APEX1605_CO_TGM")
OPC.input_sa92_TGM <- lapply(OPC.files_sa92_TGM, function(file) {
  fread(file, skip = 2, nrows = 6,
        sep = " ", fill = TRUE)
})



OPC.input_df_sa20 <- rbindlist(OPC.input_sa20, idcol = TRUE) %>% 
  setNames(OPC_col.names) %>%
  select(ID, apex.code, apex.input)

OPC.input_df_sa92_AGM <- rbindlist(OPC.input_sa92_AGM, idcol = TRUE) %>% 
  setNames(OPC_col.names) %>%
  select(ID, apex.code, apex.input)

OPC.input_df_sa92_TGM <- rbindlist(OPC.input_sa92_TGM, idcol = TRUE) %>% 
  setNames(OPC_col.names) %>%
  select(ID, apex.code, apex.input)



Pasture_names_sa20 <- sapply(strsplit(OPC.files_sa20, "_"), function(x) x[3]) %>%
  data.frame() %>% mutate(ID = row_number()) %>% rename(Pasture =".")

Pasture_names_sa92_AGM <- sapply(strsplit(OPC.files_sa92_AGM, "_"), function(x) x[3]) %>%
  data.frame() %>% mutate(ID = row_number()) %>% rename(Pasture =".")

Pasture_names_sa92_TGM <- sapply(strsplit(OPC.files_sa92_TGM, "_"), function(x) x[3]) %>%
  data.frame() %>% mutate(ID = row_number()) %>% rename(Pasture =".")



OPC.input_df_sa20 <- merge(OPC.input_df_sa20, Pasture_names_sa20, by = "ID") %>%
  mutate(source = "20 subareas") %>%
  select(-ID)

OPC.input_df_sa92_AGM <- merge(OPC.input_df_sa92_AGM, Pasture_names_sa92_AGM, by = "ID") %>%
  mutate(source = "92 subareas") %>%
  select(-ID)

OPC.input_df_sa92_TGM <- merge(OPC.input_df_sa92_TGM, Pasture_names_sa92_TGM, by = "ID") %>%
  mutate(source = "92 subareas") %>%
  select(-ID)


OPC.input_df <- rbind(OPC.input_df_sa20, OPC.input_df_sa92_AGM, OPC.input_df_sa92_TGM) %>%
  group_by(Pasture, apex.code, source) %>%
  summarize(apex.input = round(mean(apex.input), 2)) %>%
  pivot_wider(names_from = source, values_from = apex.input) %>%
  mutate(diff = `20 subareas` - `92 subareas`)

write.csv(OPC.input_df, "D:/APEX data and scripts/Data/OPC.input_compare.csv")


