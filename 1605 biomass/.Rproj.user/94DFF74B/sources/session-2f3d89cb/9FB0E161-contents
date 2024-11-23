# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(data.table) # For fast data reading and manipulation

# Define the file path for the APEX simulation output
path <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_all92/CONUNN_TGM.cag"

# Read in the biomass simulation data
# - Skip the first 9 lines of metadata
# - Retain only relevant columns: ID, year (Y), month (M), day (D), plant species (CPNM), and biomass ('AB_DDMkg/ha')
# - Filter data to only include years 2014-2018
# - Add a new column for date and ensure 'ID' is treated as an integer
biomass_cageSim_spatVar <- fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
  select(ID, Y, M, D, CPNM, 'AB_DDMkg/ha') %>%
  filter(Y %in% c(2014:2018)) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         ID = as.integer(ID)) %>%
  rename(DDMkg_ha = 'AB_DDMkg/ha') # Rename biomass column for easier referencing

# Read in the pasture metadata
# - This file links spatial subareas to specific pastures and ecological sites
PastureID_sa92 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

# Merge the biomass data with pasture metadata using the 'ID' column
biomass_cageSim_spatVar_v02 <- left_join(biomass_cageSim_spatVar, PastureID_sa92,
                                         by = "ID")

# Filter and calculate cumulative biomass accumulation
biomass_cageSim_spatVar_v03 <- biomass_cageSim_spatVar_v02 %>%
  # Keep only data up to August 12th for each year
  filter(Date <= ymd(paste(Y, "08", "12", sep = "-"))) %>%
  group_by(Y, ID, CPNM) %>%
  # Compute cumulative biomass (acc_DDM) for each year and subarea
  mutate(acc_DDM = cumsum(DDMkg_ha)) %>%
  # Filter to retain only rows corresponding to August 12th and a specific treatment (e.g., "TRM")
  filter(month(Date) == 8 & day(Date) == 12, Treatment == "TRM") %>%
  ungroup() %>%
  mutate(Plot = as.factor(Plot)) %>%
  select(-DDMkg_ha) # Drop the original biomass column to simplify the dataset

# (Optional) Save the processed data to a CSV file for further analysis or sharing
# write.csv(biomass_cageSim_spatVar_v03, "C:/APEX data and scripts/Data/SeanD_TRM sim biomass_11132024.csv")

# Plot the cumulative biomass accumulation
# - Use a line plot with 'Date' on the x-axis and cumulative biomass ('acc_DDM') on the y-axis
# - Color lines by plot and create separate panels for each combination of pasture and plant species
ggplot(biomass_cageSim_spatVar_v03) +
  geom_line(aes(x = Date, y = acc_DDM, color = Plot)) +
  facet_grid(Pasture~CPNM) + # Facet by pasture and plant species
  theme_bw() # Apply a clean theme to the plot
