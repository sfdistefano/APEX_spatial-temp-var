library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggsci)

# Function to process and summarize biomass data
summarize_biomass <- function(metadata_path, data_path, simulation_label) {
  
  # Load metadata
  metadata <- read.csv(metadata_path)
  
  # Load and process simulation data
  biom_data <- fread(data_path, fill = TRUE, skip = 9, header = TRUE) %>%
    filter(Y %in% 2014:2018,
           M %in% 5:10,
           CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
    select(ID, Y, M, D, CPNM, STL, STD) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")))
  
  # Merge with metadata and summarize
  biom_summary <- biom_data %>%
    merge(metadata, by = "ID") %>%
    mutate(cpnm_biom = (STL + STD) * 1000) %>%
    group_by(Date, Treatment, Pasture, CPNM) %>%
    summarize(cpnm_biom_pasture = mean(cpnm_biom), .groups = "drop") %>%
    group_by(Date, Treatment, Pasture) %>%
    summarize(tot_biom_pasture = sum(cpnm_biom_pasture), .groups = "drop") %>%
    group_by(Date, Treatment) %>%
    summarize(total_biom = mean(tot_biom_pasture),
              sd_total_biom = sd(tot_biom_pasture), 
              .groups = "drop") %>%
    mutate(Simulation = simulation_label)
  
  return(biom_summary)
}

# Process both datasets using the function
SimBiom_var_treatment <- summarize_biomass(
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv",
  data_path = "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.sad",
  simulation_label = "Spatial Variability"
)

SimBiom_base_treatment <- summarize_biomass(
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv",
  data_path = "C:/01-APEX1605_CO_baseline/WtGain_Simulation/APEX1605_CO_all20/CONUNN_TGM.sad",
  simulation_label = "No Variability"
)

# Combine datasets
SimBiom <- rbind(SimBiom_var_treatment, SimBiom_base_treatment)

# Plot results
biomass_plot <- ggplot(SimBiom, aes(x = Date, y = total_biom, 
                                    color = Treatment, linetype = Simulation)) +
  geom_line(linewidth = 1) +
  facet_wrap(~year(Date), ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") +
  theme_bw() +
  scale_color_npg() +
  labs(y = "Total Aboveground Biomass (kg/ha)") +
  theme(legend.position = "bottom",
        text = element_text(family = "serif", size = 20))

# Save the plot as a PNG file in portrait mode
setwd("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 biomass")

# ggsave(filename = "total_biomass_plot.png", 
#        plot = biomass_plot, 
#        width = 10, height = 11, units = "in", dpi = 300)