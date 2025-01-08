library(ggsci)

# Filter out data where CPNM is ATCA or SSHB
biomass_summary_filtered <- biomass_summary %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))

observed_data_filtered <- observed_data_v04 %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))

# Create ggplot object to visualize accumulated biomass (DDMkg_ha) by Treatment for each CPNM and Year combination
biomass_plot <- ggplot(biomass_summary_filtered, 
                       aes(x = month_day, 
                           y = mean_DDMkg_ha, 
                           color = Treatment, 
                           group = Treatment)) +
  geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), 
                  ymax = mean_DDMkg_ha + sd_DDMkg_ha, 
                  fill = Treatment), 
              alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(data = observed_data_filtered,
             aes(x = month_day,
                 y = MeankgPerHa,
                 color = Treatment,
                 shape = Treatment),
             size = 4, alpha = 1, stroke = 1.2, fill = NA) +
  scale_shape_manual(values = 1:2) +
  scale_color_npg() +
  scale_fill_npg() +
  geom_errorbar(data = observed_data_filtered,
                aes(x = month_day,
                    y = MeankgPerHa,
                    ymin = MeankgPerHa - SDkgPerHa,
                    ymax = MeankgPerHa + SDkgPerHa,
                    color = Treatment), linewidth = 0.7) +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                   labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(title = "Accumulated Biomass w/ Spatial Variability",
       x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Grazing Treatment",
       fill = "Grazing Treatment",
       shape = "Grazing Treatment") +
  theme_minimal(base_family = "serif") +
  theme(
    strip.text = element_text(size = 12, face = "bold", family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "serif", size = 12),
    axis.text.y = element_text(family = "serif", size = 12),
    axis.title = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 12),
    legend.title = element_text(family = "serif", size = 14),
    plot.title = element_text(size = 16, face = "bold", family = "serif"),
    legend.position = "bottom"
  )

# Display the plot
print(biomass_plot)

# Save the plot to a .png file
# ggsave(filename = "biomass_plot.png", plot = biomass_plot, width = 12, height = 8, dpi = 300)
