composition <- composition %>% 
  filter(!Treatment == "XC")

#### Load trait data ####

# compile CWM an Fdiv trait values
# load imputation files for each traits for all species

trait_C <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_C.csv", header =TRUE, sep = ";", dec = ",") %>%
  mutate(Species = paste0(Site, "_", Species)) %>%
  select( Site, Species, predictValue, predictionSE) #, predictionSE
trait_N <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_N.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_CN <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_CN.ratio.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_SLA <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_SLA.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_Lth <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_Lth_ave.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_LDMC <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_LDMC.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_logLA <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_logLA.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)
trait_logHeight <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/imputation_files/speciesSitePredictions_logHeight.csv", header =TRUE, sep = ";", dec = ",") %>%
  select(predictValue, predictionSE)

Species_traits <-bind_cols(trait_C, trait_N, trait_CN, trait_SLA,
                           trait_Lth, trait_LDMC, trait_logLA, trait_logHeight)%>%
  rename(C = predictValue, N = predictValue1, CN = predictValue2, SLA =
           predictValue3, Lth = predictValue4, LDMC = predictValue5, LA =
           predictValue6, Height = predictValue7)

## check distribution of imputation values of traits
ggplot(Species_traits, aes(SLA)) +
  geom_density() +
  facet_wrap(~Site)
## Distribution of imputed values within range of measured traits

## Checked SE values for each trait- Emp_nig, Emp_her, Ver_alp and Sil_vul very large SE remove from dataset
Species_traits <- Species_traits %>%
  filter(!grepl("Emp_",  Species))%>%
  filter(!grepl("Ver_alp",  Species))%>%
  filter(!grepl("Sil_vul",  Species)) %>% 
  mutate(Site = plyr::mapvalues(Site, from = dict_Site$old, to = dict_Site$new)) %>%
  select(siteID = Site, speciesID = Species, C, N, CN, SLA, Lth, LDMC, LA, Height) %>% 
  mutate(speciesID = substr(speciesID, 5, n()),
         speciesID = gsub("_", ".", speciesID),
         speciesID = paste0(siteID, "_", speciesID))



# calculation of CWM and FDvar; Community weighted mean and community-weighted variance of trait values
# join imputed traits with species cover
# wt.var() calculate weighted variance

community_cover_1516 <- composition %>%
  mutate(speciesID = paste0(siteID,"_", species))

community_FD <- left_join(community_cover_1516, Species_traits, by = c("speciesID", "siteID")) %>%
  select(siteID, turfID, Year, species, functionalGroup, Treatment, cover, graminoidCov, forbCov, bryophyteCov, C, N, CN, SLA, Lth, LDMC, LA, Height) %>%
  group_by(turfID, functionalGroup, siteID) %>%
  filter(!is.na(cover),
         Year == 2017,
         !Treatment == "XC") %>%
  mutate(richness = sum(n_distinct(species))) %>%
  mutate(diversity = diversity(cover, index = "shannon")) %>%
  mutate(evenness = (diversity/log(richness)))%>%
  mutate(sumcover = sum(cover),
         richness = mean(richness),
         diversity = mean(diversity),
         evenness = mean(evenness),
         wmean_LDMC= weighted.mean(LDMC, cover, na.rm=TRUE),
         wmean_Lth= weighted.mean(Lth, cover, na.rm=TRUE),
         wmean_LA= weighted.mean(LA, cover, na.rm=TRUE),
         wmean_SLA= weighted.mean(SLA, cover, na.rm=TRUE),
         wmean_Height= weighted.mean(Height, cover, na.rm=TRUE),
         wmean_CN = weighted.mean(CN, cover, na.rm=TRUE),
         wmean_C = weighted.mean(C, cover, na.rm=TRUE),
         wmean_N = weighted.mean(N, cover, na.rm=TRUE),
         cwv_LDMC= wt.var(LDMC, cover),
         cwv_Lth= wt.var(Lth, cover),
         cwv_LA= wt.var(LA, cover),
         cwv_SLA= wt.var(SLA, cover),
         cwv_Height= wt.var(Height, cover),
         cwv_C = wt.var(C, cover),
         cwv_N = wt.var(N, cover),
         cwv_CN = wt.var(CN, cover)) %>%
  #gather(key= Trait, value= value, -c(turfID:Year))%>%
  ungroup() %>% 
  select((siteID:bryophyteCov), (richness:cwv_CN), -Year, -species, -cover) %>%
  distinct(turfID, functionalGroup, .keep_all = TRUE)

community_FD <- community_FD %>% 
  mutate(tempLevel = recode(siteID, Ulvhaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjellingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedal = 10.5)) %>%
  mutate(temp7010 = recode(siteID, Ulvhaugen=6.17, Lavisdalen=6.45, Gudmedalen=5.87, Skjellingahaugen=6.58, Alrust=9.14, Hogsete=9.17, Rambera=8.77, Veskre=8.67, Fauske=10.3, Vikesland=10.55, Arhelleren=10.60, Ovstedal=10.78)) %>%
  mutate(precip7010= recode(siteID, Ulvhaugen=596, Lavisdalen=1321, Gudmedalen=1925, Skjellingahaugen=2725, Alrust=789, Hogsete=1356, Rambera=1848, Veskre=3029, Fauske=600, Vikesland=1161, Arhelleren=2044, Ovstedal=2923)) %>%
  mutate(precipLevel = recode(siteID, Ulvhaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjellingahaugen = 2700, Veskre = 2700, Ovstedal = 2700))

# make anomalies from control plots

community_FD <- community_FD %>% 
  filter(Treatment %in% c("C", "FGB", "GF", "GB", "FB")) %>% 
  distinct(siteID, turfID, Treatment, .keep_all = TRUE) %>% 
  mutate(blockID = substr(turfID, 4,4)) %>% 
  select(-(cwv_LDMC:cwv_CN), -functionalGroup) %>% 
  gather(c(richness: wmean_N), key = "trait", value = "value")

community_FD  %>% group_by(turfID, trait, Treatment, siteID, blockID) %>% 
  left_join(community_FD %>% filter(Treatment == "C") %>% ungroup() %>% select(Cvalue = value, siteID, blockID, trait)) %>%
  mutate(valueAnom = value - Cvalue) 
  

#------------ PLOTTING --------------

