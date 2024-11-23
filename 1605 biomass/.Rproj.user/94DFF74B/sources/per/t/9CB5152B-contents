# Create ggplot object to visualize accumulated biomass (DDMkg_ha) by Treatment for each CPNM and Year combination
biomass_plot <- ggplot(biomass_summary, aes(x = month_day, y = mean_DDMkg_ha, color = Treatment, group = Treatment)) +
  geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Treatment), alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(data = observed_data_v04, aes(x = format(Date, "%m-%d"), y = MeankgPerHa, color = Treatment), size = 3, shape = 16) +
  geom_errorbar(data = observed_data_v04, aes(x = format(Date, "%m-%d"), y = MeankgPerHa, ymin = MeankgPerHa - SDkgPerHa, ymax = MeankgPerHa + SDkgPerHa, color = Treatment), width = 0.2) +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(title = "Accumulated Biomass Across Treatments, Years, and Plant Functional Groups",
       x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Grazing Treatment",
       fill = "Grazing Treatment") +
  theme_minimal(base_family = "serif") +
  theme(
    strip.text = element_text(size = 10, face = "bold", family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "serif"),
    legend.position = "bottom"
  )

# Display the plot
print(biomass_plot)

