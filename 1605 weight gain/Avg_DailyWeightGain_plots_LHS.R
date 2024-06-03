library(tidyverse)
library(reshape2)
library(ggthemes)
library(scales)
library(data.table)

PastureID <- read.csv("D:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

## AGZ file column names
name.agz <- c("SA", "ID", "Year", "Year_num", "CPNM",
              "GNAM", "HERD", "OWNER","GZNB", "GZD", "GZSL", 
              "GZSD", "HAY", "HAYd", "WTG",
              "WSAha", "PSTLkg/ha", "PSTDkg/ha", "PSTTkg/ha",
              "POP", "PEAK")
## Importing LHS data (n = 1000)
agz_base <- read.delim("D:/01-APEX1605_CO_baseline/APEX1605_CO_all20/CONUNN_TGM.AGZ", 
                           skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year >= 2014) %>%
  mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  group_by(Year, Treatment) %>%
  summarize(MeanWT = mean(WTG)) 

agz_files <- list.files(path = "D:/LHS_all20_AGZ", 
                        pattern = "\\.AGZ$", full.names = TRUE)

all20_agz_lhs <- data.frame()

for (file in agz_files) {
  
  # Print a message indicating the file being imported
  cat("Importing file:", file, "\n")
  
  # Read the data from the file
  agz_data <- read.delim(file, skip = 10, header = FALSE, 
                         sep = "", col.names = name.agz)
  
  # Add an identifier column with the file name
  agz_data$FileID <- basename(file)
  
  # Filter the data
  agz_filtered <- agz_data %>%
    filter(Year >= 2014) %>%
    mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM"))
  
  # Calculate the summary statistics
  agz_summary <- agz_filtered %>%
    group_by(Year, Treatment, FileID) %>%
    summarize(MeanWT = mean(WTG))
  
  # Append the summary to the results data frame
  all20_agz_lhs <- rbind(all20_agz_lhs, agz_summary)
}

all20_agz_lhs_summ <- all20_agz_lhs %>%
  group_by(Year, Treatment) %>%
  summarize(MeanWT_year = mean(MeanWT),
            stdevWT = sd(MeanWT)) %>%
  rename(MeanWT = MeanWT_year) %>%
  mutate(Type = "Simulated: Montecarlo")

## Measured data
wtg <- fread("D:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2022.csv") %>%
  mutate(gain = ADG / 2.2046) %>%
  merge(PastureID, by = 'Pasture',
        all.x = TRUE)

gain <- wtg %>%
  mutate(Treatment = ifelse(is.na(Treatment), "CARM", "TRM")) %>%
  select(Year, Treatment, gain) %>% 
  group_by(Year, Treatment) %>%
  dplyr::summarise(MeanWT = mean(gain, na.rm = T),
                   stdevWT = sd(gain, na.rm = T)) %>%
  mutate(Type = "Measured")

## Simulated data (20 subareas)
agz <- read.table(file = "D:/01-APEX1605_CO_baseline/APEX1605_CO_all20/CONUNN_TGM.agz", 
                  header = F, skip = 9, col.names = name.agz)

gain_sim <- agz %>% 
  filter(Year >= 2014) %>%
  select(ID, Year, WTG) %>%
  mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  group_by(Year, Treatment) %>%
  dplyr::summarise(MeanWT = mean(WTG,na.rm = T), stdevWT = 0) %>% 
  mutate(Type = "Simulated: Original")

## Combining all data
gain_comb <- all20_agz_lhs_summ %>%
  rbind(gain, gain_sim)

## Visualizes comparisons
ggplot(gain_comb, aes(x = Treatment, y = MeanWT, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = MeanWT - stdevWT, ymax = MeanWT + stdevWT,
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
  newtheme +
  theme(text = element_text(size = 20, family = 'serif'))
