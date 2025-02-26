# Load required libraries
library(data.table)   # For fast and efficient data manipulation
library(dplyr)        # For data wrangling using pipes and verbs
library(ggplot2)      # For creating publication-quality visualizations
library(lubridate)    # For simplifying date and time handling

# Read metadata
metadata <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
# Metadata file contains details about ecological sites for various pasture IDs.

# File path and settings
file_path <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_all92/CONUNN_TGM.cag"
skip_lines <- 9
years <- 2014:2018
# Defining the file path to the evapotranspiration data and filtering parameters.

# Read and preprocess evapTransp data
evapTransp_data <- fread(file_path, fill = TRUE, skip = skip_lines, header = TRUE) %>%
  select(ID, Y, M, D, CPNM, PET, AET, AT, AE) %>%  # Keep relevant columns
  filter(Y %in% years) %>%                         # Filter for the desired years
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),    # Create a date column
         ID = as.integer(ID))                      # Ensure ID is an integer for joins

# Prepare AT data
at_data <- evapTransp_data %>%
  select(-PET, -AET, -AE) %>%              # Exclude irrelevant columns
  distinct() %>%                           # Remove duplicates
  left_join(metadata, by = "ID") %>%       # Add metadata for ecological site info
  filter(!(is.na(Ecosite)))                # Filter out rows without valid ecosites

# Calculate biweekly start date for grouping
at_data <- at_data %>%
  mutate(Biweek = Date - (wday(Date) - 1) %% 14)
# Calculate biweekly periods for grouping data.

# Calculate biweekly average AT for each CPNM
at_data_biweekly <- at_data %>%
  group_by(Biweek, Ecosite, CPNM) %>%  # Group by biweekly period, ecosite, and cover type
  summarize(AT_biweekly_avg = mean(AT, na.rm = TRUE), .groups = "drop")
# Compute the actual transpiration (AT) for each group.

# Filter for each CPNM group separately
at_data_wspg <- at_data_biweekly %>%
  filter(CPNM == "WSPG")    # Warm season perennial grasses

at_data_cspg <- at_data_biweekly %>%
  filter(CPNM == "CSPG")    # Cool season perennial grasses

at_data_forb <- at_data_biweekly %>%
  filter(CPNM == "FRB3")    # Forbs

at_data_vuoc <- at_data_biweekly %>%
  filter(CPNM == "VUOC")    # Vines and other cover types

# Plot for WSPG
ggplot(at_data_wspg, aes(x = Biweek, y = AT_biweekly_avg, color = Ecosite)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "WSPG", color = "Ecological Site",
       x = "Year", y = "Actual Evapotranspiration") +
  theme_minimal(base_family = "serif") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  
# Plot for CSPG
ggplot(at_data_cspg, aes(x = Biweek, y = AT_biweekly_avg, color = Ecosite)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "CSPG", color = "Ecological Site",
       x = "Year", y = "Actual Evapotranspiration") +
  theme_minimal(base_family = "serif") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# Plot for FORB
ggplot(at_data_forb, aes(x = Biweek, y = AT_biweekly_avg, color = Ecosite)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "FORB", color = "Ecological Site",
       x = "Year", y = "Actual Evapotranspiration") +
  theme_minimal(base_family = "serif") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# Plot for VUOC
ggplot(at_data_vuoc, aes(x = Biweek, y = AT_biweekly_avg, color = Ecosite)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "VUOC", color = "Ecological Site",
       x = "Year", y = "Actual Evapotranspiration") +
  theme_minimal(base_family = "serif") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

##### COMPARING FOR AE ACROSS ECOLOGICAL SITES #################################
ae_data <- evapTransp_data %>%
  select(-CPNM, -PET, -AET, -AT) %>%       # Exclude columns not needed for AE analysis
  distinct() %>%
  left_join(metadata, by = "ID") %>%
  filter(!(is.na(Ecosite)))                # Exclude rows without valid ecosite data

ae_data_ecosite <- ae_data %>%
  filter(year(Date) >= 2014) %>%          # Filter data for years 2014 and beyond
  group_by(Date, Ecosite) %>%             # Group by date and ecosite
  summarize(AE_ecosite = mean(AE)) %>%
  mutate(Biweek = Date - (wday(Date) - 1) %% 14)
# Calculate AE averages per ecological site and group by biweekly periods.

# Calculate biweekly averages for AE across ecosites
ae_data_biweekly <- ae_data_ecosite %>%
  group_by(Biweek, Ecosite) %>%
  summarize(AE_biweekly_avg = mean(AE_ecosite, na.rm = TRUE), .groups = "drop")

ggplot(ae_data_biweekly, aes(x = Biweek)) +
  geom_line(aes(y = AE_biweekly_avg, color = Ecosite)) +
  labs(title = "Biweekly Changes in AE by Ecological Site") +
  theme_minimal()

# Performing same plot generation for a subset of data (May-Oct of 2014)
evapTransp_data_2014 <- evapTransp_data %>% filter(year(Date) <= 2014,
                                                   month(Date) %in% c(5:10))

ae_data_2014 <- evapTransp_data_2014 %>%
  select(-CPNM, -PET, -AET, -AT) %>%       # Exclude columns not needed for AE analysis
  distinct() %>%
  left_join(metadata, by = "ID") %>%
  filter(!(is.na(Ecosite)))                # Exclude rows without valid ecosite data

ae_data_ecosite_2014 <- ae_data_2014 %>%
  filter(year(Date) >= 2014) %>%          # Filter data for years 2014 and beyond
  group_by(Date, Ecosite) %>%             # Group by date and ecosite
  summarize(AE_ecosite = mean(AE)) 

ggplot(ae_data_ecosite_2014, aes(x = Date)) +
  geom_line(aes(y = AE_ecosite, color = Ecosite)) +
  labs(title = "Daily Changes in AE by Ecological Site for 2014") +
  scale_x_date(breaks = "1 month", date_labels = "%m") +
  labs(y = "Actual Evaporation") +
  theme_minimal()

##### COMPARING FOR AET AND PET ACROSS SIMULATION AND ECOLOGICAL SITE ##########
aet_pet_data <- evapTransp_data %>%
  select(-CPNM, -AT, -AE) %>%
  distinct() %>%
  group_by(Date) %>%
  summarize(PET_cper = mean(PET), AET_cper = mean(AET))
# Summarize PET and AET across dates.

ggplot(aet_pet_data, aes(x = Date)) +
  geom_line(aes(y = PET_cper, color = "PET")) +
  geom_line(aes(y = AET_cper, color = "AET")) +
  theme_minimal(base_family = "serif") +
  labs(x = "Date", y = "Potential and Actual ET (mm/day)", color = "") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), legend.text = element_text(size = 12))
