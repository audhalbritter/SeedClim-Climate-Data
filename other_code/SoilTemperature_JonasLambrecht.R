# Prepare soil temperture file for Jonas Lambrecht.

library("writexl")


SoilTemperature <- temperature2 %>% 
  filter(logger == "tempsoil") %>% 
  filter(!is.na(value)) %>% 
  select(site, date, value, flag) %>% 
  rename("Plotcode" = "site", "Date" = "date", "Temperature" = "value", "Flag" = "flag") %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         Time = format(Date, "%H:%M")) %>% 
  select(Plotcode, Date, Year, Month, Day, Time, Temperature, Flag)
#write_xlsx(SoilTemperature, path = "SoilTemperature.xlsx", col_names = TRUE)
save(SoilTemperature, file = "SoilTemperature.Rdata")

# Extract start and end dates
SoilTempDates <- SoilTemperature %>% 
  group_by(Plotcode) %>% summarise(Start = min(Date), End = max(Date)) %>% 
  mutate(Start_date_year = year(Start),
         Start_date_month = month(Start),
         Start_date_day = day(Start),
         End_date_year = year(End),
         End_date_month = month(End),
         End_date_day = day(End))


load(file = "sites.Rdata")
SoilTemperature_metadata <- sites %>% 
  select(siteID, latitude, longitude, `altitude(DEM)`, geology) %>% 
  rename("Plotcode" = "siteID",  "Latitude" = "latitude", "Longitude" = "longitude", "Elevation" = `altitude(DEM)`, "Geology" = "geology") %>% 
  mutate(Sensor_used = "cDelta T GP1 loggers",
         Sensor_accuracy = NA,
         Sensor_notes = NA,
         Sensor_depth = -5) %>% 
  left_join(SoilTempDates, by = "Plotcode") %>% 
  mutate(Temporal_resolution = 60,
         Species_composition = "yes",
         Species_trait = "yes",
         Plot_size = "Site_level",
         Forest_canopy_cover = 0,
         Total_vegetation_cover = "X",
         Moss_layer_depth = "X",
         Leaf_area_index = NA,
         Vegetation_height = "X",
         Habitat_type = "Potentillo-Festucetum ovinae",
         Slope = "X",
         Aspect = "X",
         Disturbance_types = "grazed_grasslands",
         Disturbance_estimates = "extensive_grazed_grassland",
         Soil_type = NA,
         Soil_moisture = "X") %>% 
  select(Plotcode:Longitude, Sensor_used:Habitat_type, Elevation, Slope:Soil_moisture, Geology)

write_xlsx(SoilTemperature_metadata, path = "SoilTemperature_metadata.xlsx", col_names = TRUE)