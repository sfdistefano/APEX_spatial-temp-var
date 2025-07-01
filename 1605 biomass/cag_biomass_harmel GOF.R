library(tidyverse)
###### PREP DATA FOR ANALYSIS ##################################################
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
  # summarize to pasture-scale
  group_by(Date, Treatment, Pasture, CPNM, Y) %>%
  summarize(biomass_pasture = round(mean(DDMkg_ha), 2)) %>%
  # Recode values in the CPNM column to new categories for clarity
  mutate(CPNM = recode(CPNM,
                       "VUOC" = "CSAG",
                       "FRB3" = "FORB")) %>%
  mutate(Sim.Type = "Spatial Variability")

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

######### FUNCTION FOR HARMEL'S MODIFICATION STATISTICS ########################
# Function for GOF stats with Modification 2
compute_mod2_stats <- function(df) {
  df <- df %>%
    mutate(
      # Assign a small default (e.g., 1) when uncertainty is NA or zero
      uncertainty = ifelse(is.na(uncertainty) | uncertainty <= 0, 1, uncertainty)
    )
  
  prob <- pnorm(df$simulated, mean = df$observed, sd = df$uncertainty)
  prob_adj <- ifelse(prob > 0.5, 1 - prob, prob)
  CF <- 1 - 2 * prob_adj
  eu2i <- CF * 0.5 * abs(df$observed - df$simulated)
  
  NSE_mod2 <- 1 - sum(eu2i^2) / sum((df$observed - mean(df$observed))^2)
  d_mod2 <- 1 - sum(eu2i^2) / sum((abs(df$simulated - mean(df$observed)) + abs(df$observed - mean(df$observed)))^2)
  RMSE_mod2 <- sqrt(mean(eu2i^2))
  MAE_mod2 <- mean(abs(eu2i))
  
  tibble(
    NSE_mod2 = NSE_mod2,
    d_mod2 = d_mod2,
    RMSE_mod2 = RMSE_mod2,
    MAE_mod2 = MAE_mod2
  )
}
##### PASTURE LEVEL STATISTICS #################################################
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


# Compute GOF metrics for each Sim.Type and CPNM
mod2_summary <- comparison_df %>%
  group_by(Sim.Type, CPNM) %>%
  group_modify(~ compute_mod2_stats(.x)) %>%
  ungroup()

# Print results
print(mod2_summary)

# Optional: Barplot of Willmott's d
ggplot(mod2_summary, aes(x = CPNM, y = d_mod2, fill = Sim.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Willmott's Index of Agreement (Modification 2)",
       y = "d (Mod 2)", x = "CPNM", fill = "Simulation Type") +
  theme_minimal()

# Optional: Barplot of NSE
ggplot(mod2_summary, aes(x = CPNM, y = NSE_mod2, fill = Sim.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Nash-Sutcliffe Efficiency (Modification 2)",
       y = "NSE (Mod 2)", x = "CPNM", fill = "Simulation Type") +
  theme_minimal()

##### PLOT LEVEL STATISTICS ####################################################
# Prepare simulated data
sim_plot_clean <- biomass_spatial_Aug12_plot %>%
  rename(simulated = cumulative_DDMkg_ha) %>%
  mutate(Y = as.integer(Y)) %>%
  select(Date, Y, Treatment, Pasture, Plot, CPNM, Sim.Type, simulated)

# Prepare observed data
obs_plot_clean <- biomass_observed_plot %>%
  rename(observed = MeankgPerHa_plot) %>%
  select(Date, Y, Treatment, Pasture, Plot, CPNM, observed, uncertainty) %>%
  mutate(Y = as.integer(Y))

# Join simulated and observed
comparison_plot_df <- merge(
  sim_plot_clean,
  obs_plot_clean,
  by = c("Date", "Y", "Treatment", "Pasture", "Plot", "CPNM")
)

# Compute stats by CPNM (only one Sim.Type at this scale)
mod2_plot_summary <- comparison_plot_df %>%
  group_by(CPNM) %>%
  group_modify(~ compute_mod2_stats(.x)) %>%
  ungroup() %>%
  mutate(Sim.Type = "Spatial Variability (Plot)")

# Output results
print(mod2_plot_summary)

# Barplot: d
ggplot(mod2_plot_summary, aes(x = CPNM, y = d_mod2, fill = Sim.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Willmott's d (Modification 2) — Plot Scale", y = "d (Mod 2)", x = "CPNM") +
  theme_minimal()

# Barplot: NSE
ggplot(mod2_plot_summary, aes(x = CPNM, y = NSE_mod2, fill = Sim.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Nash–Sutcliffe Efficiency (Modification 2) — Plot Scale", y = "NSE (Mod 2)", x = "CPNM") +
  theme_minimal()


