# Define a function to summarize biomass data by ecological site
summarize_biomass_ecosite <- function(data) {
  data %>%
    # Group data by treatment, ID, plant community (CPNM), and year
    group_by(Ecosite, ID, CPNM, Y) %>%
    # Arrange data by date and calculate cumulative biomass for each observation
    arrange(Date) %>%
    mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha),
           month_day = format(Date, "%m-%d")) %>%
    ungroup() %>%
    # Group by date, treatment, pasture, plant community, year, and month-day
    group_by(Date, Ecosite, CPNM, Y, month_day) %>%
    # Calculate the mean cumulative biomass for each Ecosite
    summarize(mean_DDMkg_ha = mean(cumulative_DDMkg_ha, na.rm = TRUE),
              sd_DDMkg_ha = sd(cumulative_DDMkg_ha)) %>%
    # Filter out specific plant community codes that are not of interest
    filter(!CPNM %in% c("ATCA", "SSHB")) %>%
    ungroup()
}

# Summarize the biomass datasets
# - Summarize cumulative biomass for each treatment and year
biomass_summary_spatial_es <- summarize_biomass_ecosite(biomass_spatial)

# Calculating 1 standard deviation within each Treatment across pastures
# - Calculate mean and standard deviation of observed data by treatment and pasture
observed_data_ecosite <- observed_data_v03 %>%
  group_by(Year, Date, Ecosite, APEXcodeFG) %>%
  summarize(SDkgPerHa = sd(MeankgPerHa_plot, na.rm = TRUE),
            MeankgPerHa = mean(MeankgPerHa_plot), .groups = 'drop') %>%
  filter(Year <= 2018) %>%
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG)

# Filter out specific plant communities not needed for further analysis
observed_data_ecosite_filtered <- observed_data_ecosite %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))

# Initialize an empty list to store plots
plot_list <- list()

# Iterate through each plant functional group
for (group in plant_functional_groups) {
  # Filter data for the current group
  biomass_summary_group <- biomass_summary_spatial_es %>% filter(CPNM == group)
  observed_data_group <- observed_data_ecosite_filtered %>% filter(CPNM == group)
  
  # Generate the plot for the current group
  plot <- ggplot(biomass_summary_group, aes(x = month_day, y = mean_DDMkg_ha)) +
    geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), 
                    ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Ecosite), 
                alpha = 0.2) +
    geom_line(linewidth = 1, aes(color = Ecosite, group = Ecosite)) +
    geom_point(data = observed_data_group,
               aes(x = month_day, y = MeankgPerHa),
               size = 4, alpha = 1, stroke = 1.2, fill = NA) +
    scale_shape_manual(values = 1:2) +
    scale_color_npg() +
    scale_fill_npg() +
    geom_errorbar(data = observed_data_group,
                  aes(x = month_day, y = MeankgPerHa, ymin = MeankgPerHa - SDkgPerHa, ymax = MeankgPerHa + SDkgPerHa),
                  linewidth = 0.7) +
    facet_grid( ~ Y, scales = "free_y") +
    scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
    labs(title = paste("Plant Group:", ifelse(group == "FRB3", "FORB", group)),
         x = "Month-Day",
         y = "Accumulated Biomass (kg/ha)",
         color = "Simulation",
         fill = "Simulation",
         shape = "Simulation") +
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
  
  # Remove legend for the first two plots
  if (group %in% c("CSPG", "WSPG")) {
    plot <- plot + theme(legend.position = "none")
  }
  
  # Add the plot to the list
  plot_list[[group]] <- plot
}

# Combine all plots into one layout
combined_plot <- plot_list[["CSPG"]] + plot_list[["WSPG"]] +
  plot_list[["FRB3"]] + plot_list[["VUOC"]]
plot_layout(ncol = 2) # Arrange in 2 columns

combined_plot

biomass_summary_group <- biomass_summary_es %>% filter(CPNM == "WSPG")
observed_data_group <- observed_data_ecosite_filtered %>% filter(CPNM == "WSPG")

ggplot(biomass_summary_group, aes(x = month_day, y = mean_DDMkg_ha)) +
  geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), 
                  ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Ecosite), 
              alpha = 0.2) +
  geom_line(linewidth = 1, aes(color = Ecosite, group = Ecosite)) +
  scale_shape_manual(values = 1:2) +
  scale_color_npg() +
  scale_fill_npg() +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                   labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(title = paste("Plant Group:", ifelse(group == "FRB3", "FORB", group)),
       x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Simulation",
       fill = "Simulation",
       shape = "Simulation") +
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

biomass_summary_group <- biomass_summary_spatial_es %>% filter(CPNM == "WSPG")
observed_data_group <- observed_data_ecosite_filtered %>% filter(CPNM == "WSPG")

# Generate the plot for the current group
ggplot(biomass_summary_group, aes(x = month_day, y = mean_DDMkg_ha, 
                                  group = interaction(Y, CPNM))) +
  # geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), 
  #                 ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Ecosite), 
  #             alpha = 0.2) +
  geom_line(linewidth = 1, aes(color = Ecosite, group = Ecosite)) +
  geom_jitter(data = observed_data_group,
              aes(x = month_day, y = MeankgPerHa,
                  color = Ecosite, shape = Ecosite),
              size = 4, alpha = 1, stroke = 1.2) +
  scale_shape_manual(values = 0:2) +
  scale_color_npg() +
  scale_fill_npg() +
  geom_errorbar(data = observed_data_group,
                aes(x = month_day, y = MeankgPerHa, 
                    ymin = MeankgPerHa - SDkgPerHa, 
                    ymax = MeankgPerHa + SDkgPerHa,
                    color = Ecosite),
                width = 50,
                linewidth = 0.7) +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                   labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(title = paste("Plant Group:", ifelse(group == "FRB3", "FORB", group)),
       x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Ecological Site",
       fill = "Ecological Site",
       shape = "Ecological Site") +
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