# Load necessary libraries for data manipulation, reading, and visualization
library(tidyverse)   # Includes functions for data wrangling, manipulation, and plotting
library(data.table)  # Fast data manipulation tools
library(ggsci)       # Provides scientific journal color palettes for ggplot
library(patchwork)
library(emmeans)

# Import pasture metadata
# - Read in CSV files containing metadata for pastures, which include IDs and ecological site information
PastureID_sa92 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
PastureID_sa20 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

# Define a function for reading and preprocessing biomass simulation data
# - This function reads biomass data from simulation output files
# - Selects relevant columns and filters the data by specified years
# - Merges with metadata to provide additional context
read_preprocess_biomass <- function(file_path, biomass_col, metadata_path, skip_lines = 9, years = 2014:2018) {
  # Read in the biomass data file, skipping unnecessary header lines
  biomass_data <- fread(file_path, fill = TRUE, skip = skip_lines, header = TRUE) %>%
    # Select relevant columns: ID, year (Y), month (M), day (D), and biomass column
    select(ID, Y, M, D, CPNM, !!sym(biomass_col)) %>%
    # Filter to include only data for specified years
    filter(Y %in% years) %>%
    # Create a date column from year, month, and day; convert ID to integer
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           ID = as.integer(ID)) %>%
    # Rename the biomass column to DDMkg_ha for consistency
    rename(DDMkg_ha = !!sym(biomass_col))
  
  # Read in the metadata file
  metadata <- read.csv(metadata_path)
  
  # Merge the biomass data with the metadata to add context about each ID
  left_join(biomass_data, metadata, by = "ID")
}

# Define a function to summarize biomass data
# - Summarizes cumulative biomass by treatment, pasture, and other factors
# - Calculates means and standard deviations for each date and treatment
summarize_biomass <- function(data) {
  data %>%
    # Group data by treatment, ID, plant community (CPNM), and year
    group_by(Treatment, ID, CPNM, Y) %>%
    # Arrange data by date and calculate cumulative biomass for each observation
    arrange(Date) %>%
    mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha),
           month_day = format(Date, "%m-%d")) %>%
    ungroup() %>%
    # Group by date, treatment, pasture, plant community, year, and month-day
    group_by(Date, Treatment, Pasture, CPNM, Y, month_day) %>%
    # Calculate the mean cumulative biomass for each pasture
    summarize(mean_DDMkg_ha_pasture = mean(cumulative_DDMkg_ha, na.rm = TRUE)) %>%
    # Group again by date, treatment, and plant community to calculate the overall mean and standard deviation
    group_by(Date, Treatment, CPNM, Y, month_day) %>%
    summarize(mean_DDMkg_ha = round(mean(mean_DDMkg_ha_pasture), 2),
              sd_DDMkg_ha = round(sd(mean_DDMkg_ha_pasture, na.rm = TRUE), 2)
    ) %>%
    # Filter out specific plant community codes that are not of interest
    filter(!CPNM %in% c("ATCA", "SSHB")) %>%
    ungroup()
}

# Read and process the two simulation datasets
# - One dataset represents biomass with spatial variability
# - The other represents biomass without spatial variability
biomass_spatial <- read_preprocess_biomass(
  file_path = "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all92/CONUNN_TGM.cag",
  biomass_col = 'AB_DDMkg/ha',
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv"
)

biomass_no_variability <- read_preprocess_biomass(
  file_path = "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all20/CONUNN_TGM.cag",
  biomass_col = 'AB_DDMkg/ha',
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv"
)

# Summarize the biomass datasets
# - Summarize cumulative biomass for each treatment and year
biomass_summary_spatial <- summarize_biomass(biomass_spatial)
biomass_summary_no_var <- summarize_biomass(biomass_no_variability)

# Importing and processing observational data
# - Read observed data, select relevant columns, create new calculated columns
# - Filter for years 2014 to 2018 and convert wide data to long format
observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", 
                          header = TRUE) %>%
  # Select columns for analysis
  select(2:13, 17) %>%
  # Calculate new variables (e.g., CSAG, WSPG)
  mutate(
    CSAG = AG, 
    WSPG = (BOBU + WSPG), 
    CSPG = C3PG
  ) %>%
  # Rename year column for consistency
  rename(Year = YearSampled) %>%
  # Filter and reshape data into a long format for analysis
  select(1:5, 9:15) %>%
  filter(Year %in% 2014:2023) %>%
  pivot_longer(
    cols = c(FORB:SS, CSAG:CSPG), 
    names_to = "APEXcodeFG", 
    values_to = "v1"
  ) %>%
  # Calculate the mean value per group and summarize by plot and pasture
  group_by(Year, Pasture, Plot, Transect, Treatment, APEXcodeFG) %>%
  summarize(v1 = mean(v1), .groups = 'drop') %>%
  group_by(Year, Treatment, Pasture, Plot, APEXcodeFG) %>%
  summarize(MeankgPerHa_plot = round(mean(v1), 2), .groups = 'drop',
            uncertainty = round((sd(v1)/mean(v1)) * 100, 2)
  ) %>%
  # Filter for specific functional groups and treatments
  filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
           Treatment %in% c("AGM", "TGM")) %>%
  # Add additional calculated fields for use in plotting
  mutate(
    Type = "Observed",
    Date = ymd(paste(Year, 8, 12, sep = "-")),
    APEXcodeFG = as.character(APEXcodeFG)
  ) 

# Updating plant codes and treatment names
# - Update plant codes and pasture/treatment names for consistency across datasets
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
  ) %>%
  # remove prescribed burn plots
  filter(!(Pasture == "19N" & Plot %in% c(5:6))) %>%
  filter(!(Pasture == "18S" & Plot %in% c(5:6)))

# Adding pasture information by merging with PastureID_sa92
# - Merge observed data with metadata to add pasture information
# - Number of observations is reduced due to some pastures being "burn plots"
observed_data_v03 <- merge(observed_data_v02, PastureID_sa92, 
                           by = c("Pasture", "Plot", "Treatment"))

# Calculating 1 standard deviation within each Treatment across pastures
# - Calculate mean and standard deviation of observed data by treatment and pasture
observed_data_v04 <- observed_data_v03 %>%
  group_by(Year, Date, Treatment, Pasture, APEXcodeFG) %>%
  summarize(MeankgPerHa_pasture = mean(MeankgPerHa_plot), .groups = 'drop') %>%
  group_by(Year, Date, Treatment, APEXcodeFG) %>%
  summarize(SDkgPerHa = round(sd(MeankgPerHa_pasture, na.rm = TRUE), 2),
            MeankgPerHa = round(mean(MeankgPerHa_pasture), 2), 
            .groups = 'drop'
  ) %>%
  filter(Year <= 2018) %>% # filtering for years of interest (2014-2018)
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG)

# Filter out specific plant types not needed for further analysis (fourwing saltbush, sub-shrubs)
observed_data_filtered <- observed_data_v04 %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))

## Create plots for both datasets
# Define plant functional groups
plant_functional_groups <- c("CSPG", "FRB3", "VUOC", "WSPG")

# Combine biomass summary datasets
biomass_summary_no_var <- biomass_summary_no_var %>% mutate(Sim.Type = "No Variability")
biomass_summary_spatial <- biomass_summary_spatial %>% mutate(Sim.Type = "Spatial Variability")

biomass_summary <- rbind(biomass_summary_no_var, biomass_summary_spatial)

# Create a named vector to map group codes to full names
group_names <- c(
  CSPG = "CSPG",
  WSPG = "WSPG",
  FRB3 = "FORB",  
  VUOC = "CSAG"
)

# Initialize an empty list to store plots
plot_list <- list()

# Iterate through each plant functional group
for (group in plant_functional_groups) {
  # Filter data for the current group
  biomass_summary_group <- biomass_summary %>% filter(CPNM == group)
  observed_data_group <- observed_data_filtered %>% filter(CPNM == group)
  
  # Generate the plot for the current group
  plot <- ggplot(biomass_summary_group, aes(x = month_day, 
                                            y = mean_DDMkg_ha, 
                                            color = Treatment, 
                                            group = Treatment,
                                            shape = Treatment)
  ) +
    geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), 
                    ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Treatment), 
                alpha = 0.2) +
    geom_line(linewidth = 1) +
    geom_point(data = observed_data_group,
               aes(x = month_day, y = MeankgPerHa, 
                   color = Treatment, ),
               size = 4, alpha = 1, stroke = 1.2, fill = NA) +
    scale_shape_manual(values = 1:2) +
    scale_color_npg() +
    scale_fill_npg() +
    geom_errorbar(data = observed_data_group,
                  aes(x = month_day, y = MeankgPerHa, 
                      ymin = MeankgPerHa - SDkgPerHa, 
                      ymax = MeankgPerHa + SDkgPerHa),
                  linewidth = 0.7) +
    facet_grid(Sim.Type ~ Y, scales = "free_y") +
    scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
    labs(
      title = paste(group_names[group]),
      x = ifelse(group %in% c("CSPG", "WSPG"), "", "Date"),
      y = ifelse(group %in% c("WSPG", "VUOC"), "", "Accumulated Biomass (kg/ha)"),
      color = "Grazing Treatment",
      fill = "Grazing Treatment",
      shape = "Grazing Treatment"
    ) +
    theme_minimal(base_family = "serif") +
    theme(
      strip.text = element_text(size = 12, face = "bold", family = "serif"),
      axis.text.x = element_text(angle = 45, hjust = 1, family = "serif", size = 12),
      axis.text.y = element_text(family = "serif", size = 12),
      axis.title = element_text(family = "serif", size = 14),
      legend.text = element_text(family = "serif", size = 12),
      legend.title = element_text(family = "serif", size = 14),
      plot.title = element_text(size = 16, face = "bold", family = "serif", hjust = 0.5),
      legend.position = "bottom"
    )
  # Conditional ylim
  if (group %in% c("CSPG", "WSPG")) {
    plot <- plot + coord_cartesian(ylim = c(0, 1250))
  } else {
    plot <- plot + coord_cartesian(ylim = c(0, 800))
  }
  
  # Remove legend for the first two plots
  if (group %in% c("CSPG", "WSPG", "FRB3")) {
    plot <- plot + theme(legend.position = "none")
  }
  
  # Add the plot to the list
  plot_list[[group]] <- plot
}

# Combine all plots into one layout
combined_plot <- plot_list[["CSPG"]] + plot_list[["WSPG"]] +
  plot_list[["FRB3"]] + plot_list[["VUOC"]] +
  plot_layout(ncol = 2)

# Display combined plot
combined_plot


# Save the combined plot
setwd("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 biomass")

# ggsave(filename = "combined_accBiomass_plots.png",
#        plot = combined_plot,
#        width = 20, height = 12)

###### PREP DATA FOR ANALYSIS ##################################################
### Select the relevant data from the 'biomass_spatial' dataset and process it
## Data at plot level
biomass_spatial_Aug12_plot <- biomass_spatial %>%
  # Group the data by specific columns to prepare for cumulative calculations
  group_by(Treatment, ID, CPNM, Y) %>%
  # Arrange the grouped data by date
  arrange(Date) %>%
  # Calculate the cumulative sum of biomass (DDMkg_ha) for each group
  mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha)) %>%
  # Remove the grouping structure to return to a regular data frame
  ungroup() %>%
  # Filter the data to include only observations from August 12
  filter(month(Date) == 8, day(Date) == 12) %>%
  # Further filter to include only specific categories of CPNM
  filter(CPNM %in% c("CSPG", "WSPG", "FRB3", "VUOC")) %>%
  # Recode values in the CPNM column to new categories for clarity
  mutate(CPNM = recode(CPNM,
                       "VUOC" = "CSAG",
                       "FRB3" = "FORB"))
## Data at pasture level
biomass_spatial_Aug12 <- biomass_spatial_Aug12_plot %>%
  # summarize to pasture-scale
  group_by(Date, Treatment, Pasture, CPNM, Y) %>%
  summarize(biomass_pasture = round(mean(cumulative_DDMkg_ha), 2)) %>%
  # Recode values in the CPNM column to new categories for clarity
  mutate(CPNM = recode(CPNM,
                       "VUOC" = "CSAG",
                       "FRB3" = "FORB")) %>%
  mutate(Sim.Type = "Spatial Variability")

### Data from baseline scenario
biomass_noVar_Aug12 <- biomass_no_variability %>%
  # Group the data by specific columns to prepare for cumulative calculations
  group_by(Treatment, Pasture, CPNM, Y) %>%
  # Arrange the grouped data by date
  arrange(Date) %>%
  # Calculate the cumulative sum of biomass (DDMkg_ha) for each group
  mutate(biomass_pasture = cumsum(DDMkg_ha)) %>%
  # Remove the grouping structure to return to a regular data frame
  ungroup() %>%
  # Filter the data to include only observations from August 12
  filter(month(Date) == 8, day(Date) == 12) %>%
  # Further filter to include only specific categories of CPNM
  filter(CPNM %in% c("CSPG", "WSPG", "FRB3", "VUOC")) %>%
  # Recode values in the CPNM column to new categories for clarity
  mutate(CPNM = recode(CPNM,
                       "VUOC" = "CSAG",
                       "FRB3" = "FORB")) %>%
  # Select only necessary columns to combine with spatial var. dataset
  select(Date, Treatment, Pasture, CPNM, Y, biomass_pasture) %>%
  mutate(Sim.Type = "No Variability")

## Combine both datasets
biomass_simulated_Aug12 <- rbind(biomass_noVar_Aug12, biomass_spatial_Aug12) %>%
  mutate(Y = as.integer(Y))

## Prepare data for comparison
biomass_observed_plot <- observed_data %>%
  # Filter the observed data for specific categories and year constraints
  filter(APEXcodeFG %in% c("CSPG", "WSPG", "FORB", "CSAG"),
         Year <= 2018) %>%
  # Recode Treatment column to align categories with the biomass data
  mutate(Treatment = recode(Treatment,
                            "TGM" = "TRM",
                            "AGM" = "CARM"),
         # Recode Pasture column for consistent naming
         Pasture = recode(Pasture,
                          "NH" = "10S")) %>%
  rename(CPNM = APEXcodeFG,
         Y = Year) %>%
  # remove prescribed burn plots
  filter(!(Pasture == "19N" & Plot %in% c(5:6))) %>%
  filter(!(Pasture == "18S" & Plot %in% c(5:6)))

# Summarize at pasture-scale
biomass_observed_pasture <- biomass_observed_plot %>%
  group_by(Date, Treatment, Pasture, CPNM, Y) %>%
  summarize(biomass_pasture = round(mean(MeankgPerHa_plot), 2),
            uncertainty = round((sd(MeankgPerHa_plot)/mean(MeankgPerHa_plot)) * 100, 2))

##### PREP for PASTURE LEVEL STATISTICS ########################################
# Prepare the simulated data
sim_data_clean <- biomass_simulated_Aug12 %>%
  rename(simulated = biomass_pasture) %>%
  select(Date, Y, Treatment, Pasture, CPNM, Sim.Type, simulated)

# Prepare the observed data
obs_data_clean <- biomass_observed_pasture %>%
  rename(observed = biomass_pasture) %>%
  select(Date, Y, Treatment, Pasture, CPNM, observed, uncertainty)

# Join simulated and observed data on shared keys
comparison_df <- merge(
  sim_data_clean,
  obs_data_clean,
  by = c("Date", "Y", "Treatment", "Pasture", "CPNM")
)

# Reordering columns for excel export
excel_export <- comparison_df %>%
  select(Y, Treatment, Pasture, CPNM, Sim.Type, 
         observed, uncertainty, simulated) %>%
  mutate(simulated = round(simulated, 2)) # round by 2 decimal points

# Export for Harmel Excel workbook
# write.csv(excel_export, "cageBiomass_comparisons_2014-2018.csv")
  


##### SIMULATION COMPARISON STATS ##############################################
library(lubridate)

summarize_biomass_pasture <- function(data) {
  data %>%
    # Group data by treatment, ID, plant community (CPNM), and year
    group_by(Treatment, ID, CPNM, Y) %>%
    # Arrange data by date and calculate cumulative biomass for each observation
    arrange(Date) %>%
    mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha),
           month_day = format(Date, "%m-%d")) %>%
    ungroup() %>%
    mutate(month = month(Date),
           day = day(Date)) %>%
    # Group by date, treatment, pasture, plant community, year, and month-day
    group_by(Date, Treatment, Pasture, CPNM, Y, month_day, month, day) %>%
    # Calculate the mean cumulative biomass for each pasture
    summarize(mean_DDMkg_ha_pasture = mean(cumulative_DDMkg_ha, na.rm = TRUE)) %>%
    ungroup()
}

# Summarize the biomass datasets
# - Summarize cumulative biomass for each treatment and year
biomass_summary_spatial_pasture <- summarize_biomass_pasture(biomass_spatial) %>% 
  mutate(Sim.Type = "Spatial Variability")
biomass_summary_no_var_pasture <- summarize_biomass_pasture(biomass_no_variability) %>% 
  mutate(Sim.Type = "No Variability")

combined_summary_pasture <- bind_rows(biomass_summary_spatial_pasture, 
                                      biomass_summary_no_var_pasture) %>%
  filter(month >= 5 & month <= 10)

# Linear model comparing both scenarios
mod_biomass <- lm(mean_DDMkg_ha_pasture ~ Sim.Type * CPNM + Y, 
                  data = combined_summary_pasture)

summary(mod_biomass)

# Pair-wise contrasts
emm_biomass <- emmeans(mod_biomass, ~ Sim.Type | CPNM)

# Post-hoc analysis of pair-wise contrasts
pairs(emm_biomass, adjust = "tukey", reverse = TRUE)
