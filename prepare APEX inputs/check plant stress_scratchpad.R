SimBiom_base <- fread(path, header = FALSE, skip = 2, 
                      col.names = name.cag, fill = TRUE) %>%
  filter(Y %in% c(2014:2023) & CPNM %in% c("WSPG", "CSPG", "VUOC", "FRB3", "SSHB")) %>%
  select(ID, Y, M, D, CPNM, STL, STD, WS, TS, MIN_STRESS, HUI) %>%
  mutate(Date = ymd(paste(Y, M, D, sep = "-")),
         Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM")) %>%
  select(-c(Y:D))

SimBiom_base_CSPG <- SimBiom_base %>% 
  filter(CPNM == "CSPG",
         Date <= "2023-01-01") %>%
  group_by(Date, Treatment) %>%
  summarize(WS = mean(WS), TS = mean(TS), 
            MIN_STRESS = mean(MIN_STRESS), HUI = mean(HUI)) 

years <- as.numeric(as.Date(paste0(2014:2023, "-01-01")))

SimBiom_base_CSPG$stress_source <- SimBiom_base_CSPG$MIN_STRESS == SimBiom_base_CSPG$WS

table(SimBiom_base_CSPG$stress_source)

ggplot(SimBiom_base_CSPG) +
  geom_line(aes(x = Date, y = WS, color = "WS")) +
  # geom_line(aes(x = Date, y = TS, color = "TS")) +
  geom_line(aes(x = Date, y = HUI, color = "HUI")) +
  facet_wrap(~Treatment, scales = "free_x", ncol = 1) +
  scale_x_date(date_breaks = "4 month", date_labels = "%m") +
  ylab("Plant Stress") +
  ggtitle("Baseline CSPG") +
  theme_bw() +
  theme(text = element_text(size = 20, family = 'serif'),
        axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = years, 
             linetype = "dashed", color = "black")

ggplot(SimBiom_base_CSPG) +
  geom_line(aes(x = Date, y = HUI), color = "red") +
  facet_wrap(~Treatment, scales = "free_x", ncol = 1) +
  ggtitle("CSPG Heat Unit Index") +
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%y") +
  theme_bw() +
  theme(text = element_text(size = 20, family = 'serif'),
                axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = years, 
             linetype = "dashed", color = "black")
