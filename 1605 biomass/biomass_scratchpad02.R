# Importing necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Importing and processing observational data
observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", header = TRUE) %>%
  # Selecting relevant columns
  select(2:13, 17) %>%
  # Creating new columns for functional group calculations
  mutate(
    CSAG = AG, 
    WSPG = (BOBU + WSPG) / 2, 
    CSPG = C3PG
  ) %>%
  # Renaming YearSampled column to Year
  rename(Year = YearSampled) %>%
  # Selecting columns of interest
  select(1:5, 9:15) %>%
  # Filtering for years 2014 to 2023
  filter(Year %in% 2014:2023) %>%
  # Converting wide data to long format
  pivot_longer(
    cols = c(FORB:SS, CSAG:CSPG), 
    names_to = "APEXcodeFG", 
    values_to = "v1"
  ) %>%
  # Grouping and summarizing by relevant factors to compute means
  group_by(Year, Pasture, Plot, Transect, Treatment, APEXcodeFG) %>%
  summarize(v1 = mean(v1), .groups = 'drop') %>%
  group_by(Year, Treatment, Pasture, Plot, APEXcodeFG) %>%
  summarize(MeankgPerHa_plot = mean(v1), .groups = 'drop') %>%
  # Filtering for specific functional groups and treatments
  filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
           Treatment %in% c("AGM", "TGM")) %>%
  # Adding new columns for additional data attributes
  mutate(
    Type = "Observed",
    Date = ymd(paste(Year, 8, 12, sep = "-")),
    APEXcodeFG = as.character(APEXcodeFG)
  ) 

# Updating plant codes and treatment names
observed_data_v02 <- observed_data %>%
  mutate(
    APEXcodeFG = recode(APEXcodeFG,
                        "SS" = "SSHB",
                        "CSAG" = "VUOC",
                        "FORB" = "FRB3"),
    Treatment = recode(Treatment,
                       "TGM" = "TRM",
                       "AGM" = "CARM"),
    Pasture = ifelse(Pasture == "NH", "10S", Pasture)
  )

# Adding pasture information by merging with PastureID_sa92
observed_data_v03 <- merge(observed_data_v02, PastureID_sa92, by = c("Pasture", "Plot", "Treatment"))

# Calculating 1 standard deviation within each Treatment across pastures and adding it as a new column
observed_data_v04 <- observed_data_v03 %>%
  group_by(Year, Date, Treatment, Pasture, APEXcodeFG) %>%
  summarize(MeankgPerHa_pasture = mean(MeankgPerHa_plot), .groups = 'drop') %>%
  group_by(Year, Date, Treatment, APEXcodeFG) %>%
  summarize(SDkgPerHa = sd(MeankgPerHa_pasture, na.rm = TRUE),
         MeankgPerHa = mean(MeankgPerHa_pasture), .groups = 'drop') %>%
  filter(Year <= 2018) %>%
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG)
