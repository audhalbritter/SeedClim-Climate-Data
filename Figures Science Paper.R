#### Figures Sciece Paper ####

meta.data <- data.frame(site = c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves",  "Skj"),
                        T_level = rep(c(3,2,1), 4),
                        P_level = c(1,1,1,2,2,2,3,3,3,4,4,4))

# select dates where all sites have climate data up until end of 2015
temperaturedata <- temperature %>% 
  filter(date > "2009-07-02 00:00:00" & date < "2015-12-31 23:00:00") %>% 
  mutate(flag = ifelse(is.na(flag), "ok", flag)) %>% 
  filter(flag != "FewData") %>% 
  filter(flag != "wrongValues") %>% 
  left_join(meta.data, by = "site")


temperaturedata %>% 
  filter(logger == "tempabove", site == "Lav") %>% 
  ggplot(aes(x = date, y = value, color = flag)) +
  geom_line() +
  facet_wrap(~site)


# Calculate monthly data
monthlyTemperaturedata <- temperaturedata %>% 
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  filter(n < 3024) %>%
  left_join(meta.data, by = "site") %>%
  mutate(T_level = factor(T_level), P_level = factor(P_level)) %>%
  select(-n, -sum)


# add missing months
full_grid <- expand.grid(logger = unique(monthlyTemperaturedata$logger), site = unique(monthlyTemperaturedata$site), date = seq(min(monthlyTemperaturedata$date), max(monthlyTemperaturedata$date), by = "month"))

monthlyTemperaturedata <- left_join(full_grid, monthlyTemperaturedata) %>% tbl_df()


monthlyAirTemp <- monthlyTemperaturedata %>% 
  filter(logger == "temp200cm") %>% 
  group_by(T_level, date) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = T_level)) +
  geom_line() +
  labs(x = "", y = "Monthly air temperature in °C") +
  scale_color_manual(name = "Temperature level", values = c("orange", "red", "darkred")) +
  theme_minimal()
ggsave("monthlyAirTemp.pdf", path = "~/Desktop")


monthlyAirGroundTemp <- monthlyTemperaturedata %>% 
  filter(logger %in% c("temp200cm", "tempabove")) %>% 
  filter(!is.na(value)) %>% 
  group_by(date, logger) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = logger)) +
  geom_line() +
  labs(x = "", y = "Monthly air temperature in °C") +
  scale_color_manual(name = "Temperature", values = c("royalblue2", "seagreen3"), labels = c("Air (2m)", "Ground (5cm)")) +
  #facet_wrap(~T_level, nrow = 3) +
  theme_minimal()
ggsave("monthlyAirVsGroundTemp.pdf", path = "~/Desktop")



### GRIDDED CLIMATE DATA
load("GriddedDailyClimateData2009-2016.RData", verbose = TRUE)

climate2 <- climate %>% 
  filter(Date > "2009-06-30 00:00:00", Date < "2015-12-31 23:00:00")

# calculate monthly values
monthlyClimate2 <- climate2 %>%
  mutate(Date = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  gather(key = logger, value = value, -Site, -Year, -Month, -Day, -Date) %>% 
  group_by(Date, logger, Site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  mutate(value = ifelse(logger == "Precipitation", sum, value)) %>% 
  select(-n, -sum) %>% 
  rename(date = Date, site = Site) %>% 
  left_join(meta.data, by = "site") %>%
  mutate(T_level = factor(T_level), P_level = factor(P_level))


monthlyPrecip <- monthlyClimate2 %>% 
  filter(logger == "Precipitation") %>% 
  group_by(P_level, date) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = P_level)) +
  geom_line() +
  labs(x = "", y = "Monthly precipitation in mm") +
  scale_color_manual(name = "Precipitation level", values = c("lightblue", "blue", "darkblue", "black")) +
  theme_minimal()
ggsave("monthlyGriddedPrecip.pdf", path = "~/Desktop")

# Filter tetraterm temperature
TetratermT <- monthlyClimate2 %>% 
  filter(logger %in% c("Temperature")) %>%
  filter(month(date) %in% c(6,7,8,9))


GridPlot <- monthlyClimate2 %>% 
  filter(logger %in% c("Precipitation")) %>%
  filter(year(date) != 2016) %>% 
  bind_rows(TetratermT) %>% 
  group_by(logger, T_level, P_level) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE), sum = sum(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(n), se_sum = sd(value, na.rm = TRUE)*sqrt(n)) %>% 
  mutate(mean = ifelse(logger == "Precipitation", sum, mean)) %>% 
  mutate(se = ifelse(logger == "Precipitation", se_sum, se)) %>% 
  select(-n, -sum, -se_sum) %>% 
  unite(mean_se, mean, se, sep = "_") %>% 
  spread(key = logger, value = mean_se) %>% 
  separate(col = Precipitation, into = c("P_mean", "P_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Temperature, into = c("T_mean", "T_se"), sep = "_", convert = TRUE) %>%
  ggplot(aes(x = P_mean, xmin = P_mean - P_se, xmax = P_mean + P_se, 
             y = T_mean,ymin = T_mean - T_se, ymax = T_mean + T_se,
             color = P_level, shape = T_level)) +
  geom_point(size = 3) +
  geom_errorbar() +
  geom_errorbarh() +
  labs(x = "Annual precipitation in mm", y = "Tetraterm temperature in °C") +
  scale_color_manual(name = "Precipitation level", values = c("lightblue", "blue", "darkblue", "black")) +
  scale_shape_manual(name = "Temperature level", values = c(17, 16, 15)) +
  theme(legend.position = "top") +
  theme_minimal()

ggsave("GriddedClimateData.pdf", GridPlot, path = "~/Desktop")

  

head(soilmoisture)
soilmoisture2 %>% 
  filter(site == "Vik") %>%
  ggplot(aes(x = date, y = value, colour = file)) +
  geom_line() 
  #+ facet_wrap(~ site)

soilmoisture2 <- soilmoisture %>% 
  filter(date > "2009-07-02 00:00:00" & date < "2015-12-31 23:00:00") %>% 
  filter(month(date) %in% c(6,7,8,9)) %>% 
  mutate(flag = NA) %>% 
  mutate(flag = ifelse(file == "Alrust_met1 (4).txt" & logger == "jordf2", "wrongValues", flag)) %>% 
  mutate(flag = ifelse(file %in% c("Vikesland_met1_Fall2016.txt", "Vikesland-GP1-9-114.txt", "Vikesland-met1-20120911.txt") & logger == "jordf2", "wrongValues", flag)) %>% 
# Check Gud, 2012, 2013 jordf1
  filter(is.na(flag)) # does not work with flag != "wrongValues"

monthlySoilmoisture <- soilmoisture2 %>% 
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  filter(n < 3024) %>%
  left_join(meta.data, by = "site") %>%
  mutate(T_level = factor(T_level), P_level = factor(P_level)) %>%
  select(-n, -sum)
  

monthlySoilmoisture <- ggplot(monthlySoilmoisture, aes(x = date, y = value, colour = P_level)) +
  geom_line() +
  labs(x = "", y = "Soilmoisture") +
  scale_color_manual(name = "Precipitation level", values = c("lightblue", "blue", "darkblue", "black")) +
  facet_wrap(~ T_level, nrow = 3) +
  theme_minimal()
  
ggsave("monthlySoilmoisture.pdf", path = "~/Desktop")  
