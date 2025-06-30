# Importing and processing observational data
# - Read observed data, select relevant columns, create new calculated columns
# - Filter for years 2014 to 2018 and convert wide data to long format
observed_data <- read.csv("C:/APEX data and scripts/Data/CPER Biomass/CARM_Biomass_Widecln_attr_2024-03-26.csv", 
                          header = TRUE) %>%
  # Select columns for analysis
  select(2:13, 17) %>%
  # Calculate new variables (e.g., CSAG, WSPG)
  mutate(
    CSAG = AG, 
    WSPG = (BOBU + WSPG), 
    CSPG = C3PG
  ) %>%
  # Rename year column for consistency
  rename(Year = YearSampled) %>%
  # Filter and reshape data into a long format for analysis
  select(1:5, 9:15) %>%
  filter(Year %in% 2014:2023) %>%
  pivot_longer(
    cols = c(FORB:SS, CSAG:CSPG), 
    names_to = "APEXcodeFG", 
    values_to = "v1"
  ) %>%
  # Calculate the mean value per group and summarize by plot and pasture
  group_by(Year, Pasture, Plot, Transect, Treatment, APEXcodeFG) %>%
  summarize(v1 = mean(v1), .groups = 'drop') %>%
  group_by(Year, Treatment, Pasture, Plot, APEXcodeFG) %>%
  summarize(MeankgPerHa_plot = round(mean(v1), 2), .groups = 'drop',
            uncertainty = round((sd(v1)/mean(v1)) * 100, 2)
  ) %>%
  # Filter for specific functional groups and treatments
  filter(APEXcodeFG %in% c("WSPG", "CSPG", "SS", "FORB", "CSAG") &
           Treatment %in% c("AGM", "TGM")) %>%
  # Add additional calculated fields for use in plotting
  mutate(
    Type = "Observed",
    Date = ymd(paste(Year, 8, 12, sep = "-")),
    APEXcodeFG = as.character(APEXcodeFG)
  ) 

# Updating plant codes and treatment names
# - Update plant codes and pasture/treatment names for consistency across datasets
observed_data_v02 <- observed_data %>%
  mutate(
    APEXcodeFG = recode(APEXcodeFG,
                        "SS" = "SSHB",
                        "CSAG" = "VUOC",
                        "FORB" = "FRB3"),
    Treatment = recode(Treatment,
                       "TGM" = "TRM",
                       "AGM" = "CARM"),
    Pasture = ifelse(Pasture == "NH", "10S", Pasture)
  )

# Adding pasture information by merging with PastureID_sa92
# - Merge observed data with metadata to add pasture information
# - Number of observations is reduced due to some pastures being "burn plots"
observed_data_v03 <- merge(observed_data_v02, PastureID_sa92, 
                           by = c("Pasture", "Plot", "Treatment"))

# Calculating 1 standard deviation within each Treatment across pastures
# - Calculate mean and standard deviation of observed data by treatment and pasture
observed_data_v04 <- observed_data_v03 %>%
  group_by(Year, Date, Treatment, Pasture, APEXcodeFG) %>%
  summarize(MeankgPerHa_pasture = mean(MeankgPerHa_plot), .groups = 'drop') %>%
  group_by(Year, Date, Treatment, APEXcodeFG) %>%
  summarize(SDkgPerHa = round(sd(MeankgPerHa_pasture, na.rm = TRUE), 2),
            MeankgPerHa = round(mean(MeankgPerHa_pasture), 2), 
            .groups = 'drop'
  ) %>%
  filter(Year <= 2018) %>% # filtering for years of interest (2014-2018)
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG)

# Filter out specific plant types not needed for further analysis (fourwing saltbush, sub-shrubs)
observed_data_filtered <- observed_data_v04 %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))