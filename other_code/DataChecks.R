#### HOW TO CHECK NEW DATA ####

# Check if soil and vegetation temperature is right
plot_climate(start_date = "2016.1.1", end_date = "2016.12.31", log = c("tempsoil", "tempabove"), inc = TRUE, SITE = "Fau")

temperature %>% 
  filter(site == "Ulv", logger %in% c("tempsoil", "tempabove"), date > "2016-01-01 12:00:00") %>% 
  distinct(file)

temperature %>% 
  filter(file == "Ulvhaugen-met1_Fall2016.txt") %>% 
  tail()

# Check mean and variance of loggers
temperature %>% 
  filter(site == "Fau", logger %in% c("tempsoil", "tempabove", "temp200cm", "temp30cm"), date > "2016-01-01 12:00:00") %>% 
  ggplot(aes(x = date, y = value, color = logger)) +
  geom_line() +
  facet_wrap(~ logger)


# Plot data
dd <- temperature %>% 
  filter(logger == "temp200cm")

g <- ggplot(dd, aes(x = date, y = value, color = site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ site) + 
  theme(legend.position="none") +
  ggtitle("Air temperature 2m")

g




daily_dat <- temperature %>% 
  mutate(day = ymd(format(date, "%Y-%m-%d"))) %>%
  select(-file, -flag) %>% 
  group_by(day, site, logger) %>% 
  summarise(mean = mean(value), min = min(value), max = max(value), diff = max - min)

daily_dat %>% 
  filter(logger == "temp200cm") %>%
  ggplot(aes(x = day, y = mean, colour = site)) + geom_line() + facet_grid(~ site)

daily_dat %>% 
  filter(logger == "temp30cm") %>% 
  ggplot(aes(x = day, y = diff, colour = site)) + geom_point() + facet_wrap(~site)
