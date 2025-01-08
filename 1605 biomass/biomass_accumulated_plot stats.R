# Select the relevant data from the 'biomass_spatial' dataset and process it
biomass_spatial_Aug12 <- biomass_spatial %>%
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

# Process observed data for comparison
observed_data_herb <- observed_data %>% 
  # Filter the observed data for specific categories and year constraints
  filter(APEXcodeFG %in% c("CSPG", "WSPG", "FORB", "CSAG"),
         Year <= 2018) %>%
  # Recode Treatment column to align categories with the biomass data
  mutate(Treatment = recode(Treatment,
                            "TGM" = "TRM",
                            "AGM" = "CARM"),
         # Recode Pasture column for consistent naming
         Pasture = recode(Pasture,
                          "NH" = "10S"))

# Merge the processed biomass and observed datasets
results_compare <- merge(biomass_spatial_Aug12, observed_data_herb,
                         # Match on specific columns to combine datasets
                         by.x = c("Date", "Treatment", "Pasture", "Plot", "CPNM"),
                         by.y = c("Date", "Treatment", "Pasture", "Plot", "APEXcodeFG"),
                         # Retain all rows from the biomass data
                         all.x = TRUE) %>%
  # Select relevant columns for the final comparison table
  select(Year, Date, Treatment, Pasture, Plot, CPNM, 
         cumulative_DDMkg_ha, MeankgPerHa_plot, uncertainty) %>%
  # Rename columns for clarity in the final output
  rename(Predicted = cumulative_DDMkg_ha,
         Observed = MeankgPerHa_plot)

# Export the comparison results to a CSV file for external analysis
# write.csv(results_compare, "APEX_results_comparison_pred_v_obs.csv")
