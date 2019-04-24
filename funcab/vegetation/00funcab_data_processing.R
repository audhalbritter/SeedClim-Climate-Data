##############################################################################
# Community data for all funcab analyses
##############################################################################
# what to do with plots of only one species?

library(tidyverse)
library(DBI)
library(dbplyr)
library(SDMTools)
library(readxl)
library(RSQLite)

con <- src_sqlite(path = "~/OneDrive - University of Bergen/Research/FunCaB/seedclim.sqlite", create = FALSE)
#con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")

source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/dictionaries.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- database.controls.import ---- 
# replace species names where mistakes have been found in database
problems <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE) %>%
  filter(!old %in% c("Vio.can", "Com.ten", "Sel.sel")) %>%
  filter(cover != "WHAT HAPPENED") %>%
  mutate(cover = as.numeric(cover))

prob.sp <- problems %>%
  filter(!is.na(Year)) %>% 
  select(-functionalGroup)

# load the dictionary merger
mergedictionary <- read.csv2(file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/mergedictionary.csv")

prob.sp.name <- problems %>% 
  filter(is.na(Year), !old %in% c("Eri.bor")) %>% 
  select(old, new) %>% 
  bind_rows(mergedictionary)

problems.cover <- filter(problems, !is.na(cover)) %>%
  select(turfID, year = Year, species = old, cover)

FG <- tbl(con, "character_traits") %>% 
  filter(trait == "functionalGroup") %>% 
  select(species, functionalGroup = value) %>% 
  collect()

my.GR.data <-tbl(con, "subTurfCommunity") %>%
  group_by(turfID, year, species) %>% 
  summarise(n_subturf = n()) %>% 
  collect() %>% 
  full_join(tbl(con, "turfCommunity") %>% collect()) %>%
  left_join(tbl(con, "taxon"), copy = TRUE) %>%
  left_join(tbl(con, "turfs"), copy = TRUE) %>%
  left_join(tbl(con, "plots"), by = c("destinationPlotID" = "plotID"), copy = TRUE) %>%
  left_join(tbl(con, "blocks"), by = "blockID", copy = TRUE) %>%
  left_join(tbl(con, "sites"), by = "siteID", copy = TRUE) %>%
  left_join(tbl(con, "turfEnvironment"), copy = TRUE) %>%
  select(siteID, blockID, plotID = destinationPlotID, turfID, TTtreat, GRtreat, Year = year, species, cover, temperature_level, precipitation_level, recorder, totalVascular, totalBryophytes, vegetationHeight, mossHeight, litter) %>%
  mutate(TTtreat = factor(TTtreat), GRtreat = factor(GRtreat)) %>%
  ungroup() %>% 
  filter(Year > 2014, TTtreat == "TTC"|GRtreat == "TTC")

my.GR.data <- my.GR.data %>%
  mutate(vegetationHeight = if_else(Year == 2015, vegetationHeight*10, vegetationHeight),
         mossHeight = if_else(Year == 2015, mossHeight*10, mossHeight))

levels(my.GR.data$TTtreat) <- c(levels(my.GR.data$TTtreat),levels(my.GR.data$GRtreat))
my.GR.data$TTtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] <- my.GR.data$GRtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] # merge the GRtreat and TTtreat into one column
my.GR.data$GRtreat <- NULL

my.GR.data$recorder[is.na(my.GR.data$recorder)] <- "unknown botanist"
my.GR.data$cover[my.GR.data$recorder == "PM"] <- my.GR.data$cover[my.GR.data$recorder=="PM"]*1.20

###--- fixes for botanist biases ---###
siri <- my.GR.data %>%
  filter(recorder == "Siri") %>%
  group_by(turfID, Year) %>%
  mutate(SumOfcover = sum(cover)) %>%
  filter(SumOfcover/totalVascular < 1.35)

siri.fix <- paste(as.character(my.GR.data$turfID), my.GR.data$Year) %in% paste(siri$turfID, siri$Year)
my.GR.data$cover[siri.fix] <- my.GR.data$cover[siri.fix]*1.3

owen <- my.GR.data %>% 
  filter(recorder == "Owen") %>% 
  group_by(turfID, Year) %>% 
  mutate(sumOfCover = sum(cover)) %>% 
  filter(sumOfCover/totalVascular > 1.5)

owen.fix <- paste(as.character(my.GR.data$turfID), my.GR.data$Year) %in% paste(owen$turfID, owen$Year)
my.GR.data$cover[owen.fix] <- my.GR.data$cover[owen.fix]/1.5

my.GR.data <- my.GR.data %>%
  filter(turfID %in% dict_TTC_turf$TTtreat) %>% # or semi_join()
  mutate(Treatment = "C", TTtreat = turfID) %>%
  left_join(dict_TTC_turf, by = "TTtreat", suffix = c(".new", "")) %>% 
  mutate(blockID = substr(turfID, 4, 4)) %>% 
  select(-c(plotID, temperature_level, precipitation_level, totalVascular, litter, turfID.new)) %>% 
  filter(!is.na(cover))


####-------- load funcab data ---------####

gudfun2015 <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_composition/funcab_Gudmedalen.xlsx", col_types = "text")

funcab_2015 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_composition/funcab_composition_2015-utenGud.csv", delim = ";", col_types = cols(.default = "c")) # ; or \t

funcab_2016 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_composition/funcab_composition_2016.csv", delim = ";", col_types = cols(.default = "c"))

funcab_2017 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_composition/funcab_composition_2017.csv", delim = ";", col_types = cols(.default = "c"))

scBryo <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_composition/2017seedclimBryophyte.xlsx")

## ---- funcab.data.import ---- 

# bind composition data, replace _ with . for compatibility in spp names
composition <- funcab_2016 %>% 
  bind_rows(funcab_2015) %>% 
  bind_rows(gudfun2015) %>% 
  bind_rows(funcab_2017) %>% 
  filter(subPlot == "%") %>% 
  select(c(siteID:subPlot), Year = year, recorder, c(totalGraminoids:mossHeight), litter, acro, pleuro, c(`Ach mil`:`Vis vul`)) %>%
  select_if(colSums(!is.na(.)) > 0) %>% 
  gather(c("Ach mil":"Vio sp"), key = "species", value = "cover") %>% 
  mutate(species = gsub("\\ |\\_", ".", species)) %>% 
  left_join(dict_TTC_turf, by = c("turfID" = "TTtreat"), suffix = c(".old", ".new")) %>%
  mutate(turfID = if_else(!is.na(turfID.new), turfID.new, turfID)) %>% 
  mutate_at(vars(cover, Year, totalGraminoids:pleuro), as.numeric) %>% 
  mutate(turfID = if_else(blockID == 16 & siteID == "Gudmedalen", gsub("16", "5", turfID), turfID),
         blockID = if_else(blockID == 16 & siteID == "Gudmedalen", gsub("16", "5", blockID), blockID),
         turfID = if_else(blockID == "3" & Year == 2015 & Treatment == "C", "Alr3C", turfID),
         turfID = recode(turfID, "Alr4FGB" = "Alr5C"),
         turfID = recode(turfID, "Lav1G " = "Lav1G"),
         blockID = if_else(turfID == "Gud12C", "12", blockID)) %>% 
  filter(!(blockID == "4" & Year == 2015 & siteID == "Alrust"),
         !(turfID =="Gud12C" & Year == 2015))


# overwrite problem spp with their correct names and covers
composition <- composition %>% 
  left_join(prob.sp, by = c("Year", "turfID", "siteID", "species" = "old"), suffix = c("", ".new")) %>%
  mutate(species = coalesce(new, species),
         cover = coalesce(cover.new, cover)) %>% 
  select(-new, -cover.new, -subPlot, - turfID.new) %>% 
  left_join(prob.sp.name, by = c("species" = "old")) %>% 
  mutate(species = if_else(!is.na(new), new, species)) %>% 
  group_by_at(vars(-cover, -new)) %>% 
  summarise(cover = sum(cover, na.rm = TRUE)) %>% 
  ungroup()

FGBs <- composition %>% 
  filter(Treatment %in% c("FGB", "GF")) %>% 
  select(-species, -cover) %>% 
  distinct() %>% 
  filter(Year > 2015)

# filter out funcab controls that are also TTCs in 2015 & 2016
ttcs1516 <- composition %>% 
  filter(Treatment == "C", !Year == 2017, !is.na(Year)) %>% 
  right_join(dict_TTC_turf) %>%
  select(-species, -cover, -pleuro, -acro, -litter) %>% 
  distinct()

ttcs17 <- composition %>% 
  filter(Treatment == "C", Year == 2017) %>% 
  right_join(dict_TTC_turf) %>%
  group_by(Year, turfID) %>% 
  mutate(sumcover = sum(cover)) %>% 
  filter(sumcover == 0) %>% 
  ungroup() %>% 
  select(-species, -cover, -sumcover, -litter) %>% 
  distinct() %>% 
  full_join(scBryo, by = "turfID", suffix = c(".old", "")) %>% 
  select(-totalBryophytes.old, -mossHeight.old, -vegetationHeight.old, -TTtreat.old)

####################
#### clean data ####

# join with TTC data
comp2 <- composition %>% 
  filter(cover > 0) %>% 
  mutate(blockID = if_else(nchar(blockID) > 1, gsub("[^[:digit:]]", "", blockID), blockID)) %>% 
  full_join(my.GR.data, by = c("siteID", "blockID", "turfID", "Treatment", "Year", "species", "recorder"), suffix = c("", ".new")) %>% 
  mutate(cover = if_else(cover == 0|is.na(cover), cover.new, cover),
         mossHeight = if_else(mossHeight == 0|is.na(mossHeight), mossHeight.new, mossHeight),
         vegetationHeight = if_else(vegetationHeight == 0|is.na(vegetationHeight), vegetationHeight.new, vegetationHeight),
         totalBryophytes = if_else(totalBryophytes == 0|is.na(totalBryophytes), totalBryophytes.new, totalBryophytes)) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -cover.new)

# rejoin funcab attributes of the TTCs in 2016 and 2017
comp2 <- comp2 %>% 
  left_join(ttcs1516, by = c("siteID", "Treatment", "turfID", "Year", "TTtreat"), suffix = c("", ".new")) %>% 
  mutate(mossHeight = if_else(mossHeight == 0|is.na(mossHeight), mossHeight.new, mossHeight),
         vegetationHeight = if_else(vegetationHeight == 0|is.na(vegetationHeight), vegetationHeight.new, vegetationHeight),
         totalBryophytes = if_else(is.na(totalBryophytes), totalBryophytes.new, totalBryophytes),
         totalGraminoids = if_else(is.na(totalGraminoids), totalGraminoids.new, totalGraminoids),
         totalForbs = if_else(is.na(totalForbs), totalForbs.new, totalForbs)) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -totalForbs.new, -totalGraminoids.new) %>%
  left_join(ttcs17, by = c("siteID", "blockID", "Treatment", "turfID", "Year", "TTtreat", "recorder", "acro", "pleuro"), suffix = c("", ".new")) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -totalForbs.new, -totalGraminoids.new) %>%
  filter(cover > 0)

comp2 <- comp2 %>% 
  mutate(mossHeight = case_when(
    turfID == 'Alr1F' & Year == 2017 ~ 0,
    turfID == 'Alr3G' & Year == 2017 ~ 8.6,
    turfID == 'Alr5F' & Year == 2017 ~ 17.5,
    turfID == 'Alr5G' & Year == 2017 ~ 17.5,
    turfID == 'Fau2F' & Year == 2017 ~ 4,
    turfID == 'Fau2G' & Year == 2017 ~ 4,
    turfID == 'Fau5F' & Year == 2017 ~ 0,
    turfID == 'Skj2F' & Year == 2017 ~ 7,
    turfID == 'Ulv3F' & Year == 2017 ~ 3,
    turfID == 'Ulv4C' & Year == 2017 ~ 0,
    turfID == 'Alr2GF' & Year == 2017 ~ 3,
    turfID == 'Hog4GF' & Year == 2017 ~ 18,
    turfID == 'Alr1C' & Year == 2017 ~ 15,
    turfID == 'Arh1F' & Year == 2017 ~ 13.25,
    TRUE ~ mossHeight),
    vegetationHeight = case_when(
      turfID == 'Alr1C' & Year == 2017 ~ 135,
      turfID == 'Alr3GB' & Year == 2017 ~ 65,
      turfID == 'Fau4F' & Year == 2017 ~ 70,
      turfID == 'Ulv3B' & Year == 2017 ~ 44.5,
      turfID == 'Ulv3GB' & Year == 2017 ~ 30,
      turfID == 'Ves1FB' & Year == 2017 ~ 65,
      turfID == 'Ves2GB' & Year == 2017 ~ 40,
      turfID == 'Ves3FB' & Year == 2017 ~ 50,
      TRUE ~ vegetationHeight),
    totalBryophytes = case_when(
      turfID == 'Alr1F' & Year == 2015 ~ 0,
      turfID == 'Alr1FGB' & Year == 2015 ~ 0,
      turfID == 'Alr1GB' & Year == 2015 ~ 0,
      turfID == 'Alr1GF' & Year == 2015 ~ 0,
      turfID == 'Alr3G' & Year == 2015 ~ 0,
      turfID == 'Fau2G' & Year == 2015 ~ 0,
      turfID == 'Ovs1C' & Year == 2015 ~ 100,
      turfID == 'Fau2C' & Year == 2015 ~ 40,
      TRUE ~ totalBryophytes),
    totalForbs = case_when(
      turfID == 'Fau2C' & Year == 2015 ~ 65,
      turfID == 'Gud12C' & Year == 2015 ~ 70,
      turfID == 'Vik2C' & Year == 2015 ~ 60,
      TRUE ~ totalForbs),
    totalGraminoids = case_when(
      turfID == 'Gud12C' & Year == 2015 ~ 22,
      turfID == 'Vik2C' & Year == 2015 ~ 30,
      TRUE ~ totalGraminoids))


comp2 <- comp2 %>% 
  group_by(turfID, Year) %>% 
  mutate(totalBryophytes = if_else(is.na(totalBryophytes), pleuro + acro, totalBryophytes)) %>% 
  ungroup() %>% 
  mutate(turfID = if_else(grepl("TTC", turfID), turfID, substring(turfID, 4, n())),
         Treatment = gsub(" ", "", Treatment),
         turfID = paste0(str_sub(siteID, 1, 3), turfID),
         species = gsub(" ", ".", species))

# functional groups
comp2 <- comp2 %>% 
  left_join(FG) %>% 
  mutate(functionalGroup = if_else(
    grepl("pteridophyte", functionalGroup), "forb", 
    if_else(grepl("woody", functionalGroup), "forb", functionalGroup)))

#fix functional group discrepancies
comp2 <- comp2 %>% 
  filter(!(Treatment %in% c("GB", "GF", "G", "FGB") & functionalGroup == "graminoid"  & Year > 2015),
         !(Treatment %in% c("FB", "GF", "F", "FGB") & functionalGroup == "forb" & Year > 2015)) %>% 
  mutate(functionalGroup = if_else(species == "Jun.sp", "graminoid",
                                   if_else(species%in% c("Ped.pal", "Pop.tre", "Arenaria", "Pilosella"), "forb", functionalGroup))) %>%
  filter(!is.na(cover)) %>% 
  rename(forbCov = totalForbs, mossCov = totalBryophytes, graminoidCov = totalGraminoids) %>% 
  bind_rows(FGBs %>% rename(forbCov = totalForbs, graminoidCov = totalGraminoids, mossCov = totalBryophytes))

# filter for  moss values from 2017
mossHeight <- comp2 %>% 
  filter(Year == 2017) %>% 
  select(turfID, mossHeight) %>%
  filter(!(is.na(mossHeight))) %>% 
  distinct(turfID, .keep_all = TRUE) %>% 
  ungroup()

# remove unwanted columns
comp2 <- comp2 %>% 
  select(-TTtreat, -blockID.new, -recorder.new, -acro, -pleuro)


# add climate info
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/weather.R")
