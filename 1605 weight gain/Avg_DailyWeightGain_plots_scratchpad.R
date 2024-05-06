agm_agz_var <- read.delim("APEX1605_CO_92 subareas_div - Copy/APEX1605_CO_AGM - Copy/CONUNN_TGM.AGZ", 
                          skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year >= 2014) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Spatial", SD = 0)

agm_dgz_var_pop <- read.delim("APEX1605_CO_92 subareas_div_dyn plant pop - Copy/APEX1605_CO_AGM - Copy/CONUNN_TGM.AGZ", 
                              skip = 10, header = FALSE, sep = "", col.names = name.agz) %>%
  filter(Year >= 2014) %>%
  group_by(Year) %>%
  summarize(MeanWT = mean(WTG)) %>%
  mutate(Type = "Spatial+Temporal", SD = 0)

AGMGain <- rbind(Gain_AGMO, agm_agz_base, agm_agz_pop, agm_agz_var, agm_dgz_var_pop)

## Plot scenario results
ggplot() +
  geom_line(data = AGMGain, aes(x = Year, y = MeanWT, 
                                color = Type)) +
  geom_point(data = AGMGain, aes(x = Year, y = MeanWT, 
                                 color = Type, shape = Type), 
             size = 3.5) +
  geom_errorbar(data = Gain_AGMO,
                aes(x = Year, y = MeanWT,
                    ymin = MeanWT - SD, 
                    ymax = MeanWT + SD, 
                    color = Type),
                width = 0.25) +
  geom_bar(data = ppt, aes(x = year, y = ppt_scale, 
                           fill = "Annual Precipitation"),
           alpha = 0.3,
           stat = 'identity') +
  scale_fill_manual(name = "Data Type", values = "steelblue") +
  scale_y_continuous("Mean Daily Weight Gain (kg/hd/day)", 
                     limits = c(0,1.3),
                     sec.axis = sec_axis(~./coeff, name = "Annual Precipitation (mm)")) +
  theme_bw() +
  scale_color_npg() +
  ggtitle("CARM")+
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 
                                2018, 2019, 2020, 2021, 2022)) +
  scale_shape_manual(values = c(16,15,17,18,16))  +
  theme(text = element_text(size = 15, family = 'serif')) +
  guides(color = guide_legend(title = "Weight Gain Data"),
         shape = guide_legend(title = "Weight Gain Data"))

#####
AGMGain_compare <- AGMGain %>%
  select(-SD) %>%
  filter(Type != "Measured") %>%
  merge(Gain_AGMO, by = "Year") %>%
  select(-SD, -Type.y) %>%
  rename(Simulated = MeanWT.x, Observed = MeanWT.y,
         Sim.Type = Type.x)  %>%
  mutate(Treatment = "CARM")

Gain_simStats <- rbind(AGMGain_compare, TGMGain_compare) %>%
  group_by(Treatment, Sim.Type) %>%
  summarise(rmse = round(rmse(Simulated, Observed), 2),
            nrmse = nrmse(Simulated, Observed, norm = "maxmin")/100,
            d = round(d(Simulated, Observed), 3),
            pbias = pbias(Simulated, Observed))
