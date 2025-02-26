ggplot(grazing_weight_data_yearly, aes(x = factor(Year), y = MeanWT, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars for mean weight
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars
  facet_wrap(~ Treatment) +  # Facet by Treatment (TRM and CARM)
  labs(x = "Year", 
       y = "Mean Daily Weight Gain (kg/head/day)", 
       fill = "Data Source") +  # Labels and legend title
  theme_minimal() +   # Clean theme
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 24))

# Print the combined plot
print(ggplot_grazing_yearly)
