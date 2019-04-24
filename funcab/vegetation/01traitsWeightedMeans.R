#----- TRAITS AND WEIGHTED MEANS ----#
# source Ragnhild's trait data
source("~/OneDrive - University of Bergen/Research/FunCaB/seedclimComm/ragnhild_trait_data/load_traits.R") # warning here is fine, it just means those spp didn't have CN data collected

traitdata <- traitdata %>% 
  select(siteID, species, Height_mean, LA_mean, SLA_mean, LDMC_mean, CN_mean, Lth_mean)


traits <- tbl(con, "taxon") %>% 
  collect() %>% 
  left_join(tbl(con, "moreTraits"), copy = TRUE, by = "species") %>% 
  select(species, seedMass)

# adding traits to my.GR.data
composition <- comp2 %>%
  left_join(traitdata, by = c("species", "siteID")) %>%
  left_join(traits, by = "species") %>% 
  mutate(functionalGroup = if_else(is.na(functionalGroup), "forb", functionalGroup))

library(vegan)

composition <- composition %>%
  group_by(turfID, Year) %>%
  mutate(richness = sum(n_distinct(species))) %>% 
  mutate(diversity = diversity(cover, index = "shannon")) %>% 
  mutate(evenness = (diversity/log(richness))) %>% 
  filter(!is.na(cover)) %>%
  group_by(turfID, siteID, Year) %>% 
  mutate(wmH = weighted.mean(Height_mean, cover, na.rm=TRUE),
         wmSM = weighted.mean(seedMass, cover, na.rm=TRUE),
         wmSLA = weighted.mean(SLA_mean, cover, na.rm=TRUE),
         wmLA = weighted.mean(LA_mean, cover, na.rm=TRUE),
         wmLDMC = weighted.mean(LDMC_mean, cover, na.rm=TRUE),
         wmLTH = weighted.mean(Lth_mean, cover, na.rm=TRUE),
         wmCN = weighted.mean(CN_mean, cover, na.rm=TRUE)) %>% #, 
  select(-Height_mean, -LA_mean, -SLA_mean, -seedMass, -Lth_mean, -LDMC_mean, -CN_mean, -TTtreat, -species, -cover) %>% 
  distinct(turfID, Year, functionalGroup, .keep_all = TRUE) %>% 
  group_by(turfID, siteID, Year) %>% 
  spread(key =functionalGroup, value = wmH) %>% 
  mutate(mossHeight = if_else(grepl("B", Treatment), 0, mossHeight),
         forb = if_else(grepl("F", Treatment), 0, forb),
         graminoid = if_else(grepl("G", Treatment), 0, graminoid)) %>% 
  ungroup()

#save(composition, file = "/Volumes/Macintosh HD/Users/fja062/Desktop/funcabComp.RData")