path <- "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/Cage Biomass Simulation/APEX1605_CO_all92/CONUNN_TGM.cag"

biomass_cageSim_spatVar <- fread(path, fill = TRUE, skip = 9, header = TRUE) %>%
  select(ID, Y, M, D, CPNM, 'AB_DDMkg/ha', STL, STD) %>%
  filter(Y %in% c(2014:2018)) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         ID = as.integer(ID)) %>%
  rename(AB_DDMkg_ha = 'AB_DDMkg/ha')

PastureID_sa92 <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")

biomass_cageSim_spatVar_v02 <- left_join(biomass_cageSim_spatVar, PastureID_sa92,
                                     by = "ID")

biomass_cageSim_spatVar_v03 <- biomass_cageSim_spatVar_v02 %>%
  filter(#month(Date) == 8 & day(Date) == 12,
         Treatment == "TRM") %>%
  mutate(Plot = as.factor(Plot),
         STL_kgha = STL * 1000,
         STD_kgha = STD * 1000,
         Biomass = STL_kgha + STD_kgha)

write.csv(biomass_cageSim_spatVar_v03, "C:/APEX data and scripts/Data/SeanD_TRM sim biomass_11132024.csv")

biomass <- ggplot(data = biomass_cageSim_spatVar_v03)+
  geom_line(aes(x = Date, y = Biomass, color = Plot)) +
  facet_grid(Pasture ~ CPNM, 
             scales = "free_y") +
  ylab(expression(paste("STL + STD (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

new_column <- ggplot(data = biomass_cageSim_spatVar_v03)+
  geom_line(aes(x = Date, y = AB_DDMkg_ha, color = Plot)) +
  facet_grid(Pasture ~ CPNM, 
             scales = "free_y") +
  ylab(expression("AB_DDMkg_ha")) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

plot <- list(biomass, new_column)
pdf('C:/APEX data and scripts/Data/SeanD_92 subareas_plot.pdf', width=10, height=10)
plot
dev.off()

