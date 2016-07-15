# MONTHLY AND DAILY TEMPERATURE

library("lubridate")
library("tidyr")
library("dplyr")
library("ggplot2")


#### Calculate monthly means ####
threshold <- 7 * 24 #~ one week. Minimum accepted

monthlyTemperature <- temperature %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  select(-n, -sum)

head(monthlyTemperature)
save(monthlyTemperature, file = "Monthly.Temperature_2008-2016.RData")

#fill missing months with NA y merging with complete dataset
filler <- expand.grid(
  site = unique(monthlyTemperature$site),
  logger = unique(monthlyTemperature$logger),
  date = seq(
    min(monthlyTemperature$date),
    max(monthlyTemperature$date),
    by = "month"
  )
)
monthlyTemperature <- merge(monthlyTemperature, filler, all = TRUE)

# Rbind gridded and Seedclim data
monthly <- rbind(monthly.temp, monthlyTemperature)
monthly$site <- factor(monthly$site, levels=c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))


monthly %>%
  filter(site == "Gud", logger %in% c("temp200cm", "gridded")) %>%
  ggplot() + geom_line(aes(x = date, y = value, group = logger, color = logger))
  

monthly %>%
  filter(logger %in% c("temp200cm", "gridded")) %>%
  filter(value < 40) %>% # should be able to take this out with the threshold thing above!!!
  ggplot(aes(x = date, y = value, color = logger, size = logger)) +
    geom_line() +
    scale_color_manual(values = c("darkgray", "lightblue")) +
    scale_size_manual(values = c(3,1)) +
    #scale_colour_brewer(type = "qual", palette="Paired") +
    facet_wrap(~site) +
    xlab("") + ylab("Monthly temperature in °C")

monthly %>%
  filter(logger %in% c("temp200cm", "temp30cm", "tempsoil", "tempabove")) %>%
  filter(value < 40) %>%
  ggplot(aes(x = date, y = value, color = logger)) +
  geom_line() +
  #scale_color_manual(values = c("blue", "lightblue")) +
  facet_wrap(~site) +
  xlab("") + ylab("Monthly temperature in °C")




#### Calculate daily means ####
dailyTemperature <- temperature %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  select(-n, -sum)

save(dailyTemperature, file = "Daily.Temperature_2008-2016.RData")


# Rbind gridded and Seedclim data
daily <- rbind(daily.temp, dailyTemperature)
daily$site <- factor(daily$site, levels=c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))

daily %>%
  filter(logger %in% c("tempsoil", "gridded")) %>%
  #filter(value < 40) %>% # should be able to take this out with the threshold thing above!!!
  ggplot(aes(x = date, y = value, color = logger, size = logger)) +
  geom_line() +
  scale_color_manual(values = c("darkgray", "lightblue")) +
  scale_size_manual(values = c(3,1)) +
  #scale_colour_brewer(type = "qual", palette="Paired") +
  facet_wrap(~site) +
  xlab("") + ylab("Daily temperature in °C")

plot_gridded_temp(data = daily, start_date = "2014.1.1", end_date = "2015.12.31", SITE = c("Skj", "Gud", "Lav", "Ves", "Ram", "Hog"), log = c("tempabove", "gridded"), inc = TRUE, breaks = "month")
