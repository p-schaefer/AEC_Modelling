# Fish Endpoints ----------------------------------------------------------
# Setup -------------------------------------------------------------------

## Load libraries 
library(tidyverse)
library(openxlsx)
library(sf)
source("R/00_Functions/Endpoint_Calc_Functions.R")

# Load Data ---------------------------------------------------------------

lookup_tbl<-read.csv(file.path("data","fish_lookup.csv")) %>% 
  mutate(name.to.use=trimws(name.to.use))
colnames(lookup_tbl)[colnames(lookup_tbl)=="OMNR.Code"]<-"SpeciesCode"

SATI_df<-read.csv(file.path("data","raw","Bio","lkpSpeciesHabitatRequirement.csv")) %>% 
  select(SpeciesCode=FWISFishCode,OPT_TEMP) %>% 
  distinct() %>% 
  filter(!is.na(OPT_TEMP))

lookup_tbl<-left_join(lookup_tbl,SATI_df,by="SpeciesCode")

raw_tbl<-read.csv(file.path("data","raw","Bio","tblFishSummaryOfTotalCatches.csv"))

raw_tbl<-raw_tbl %>% 
  filter( 
    OSAPSE==1, #the sample event was associated with at least one OSAP project and the sample event itself used site boundaries that were defined as per OSAP
    !is.na(TotalWeightPer100m2) # This makes sure only valid single taxa are included (i.e., Cyprid bulk samples will be excluded)
  )

# Setup Sample Event table ------------------------------------------------

SampleEventID<-raw_tbl[!is.na(raw_tbl$UsableArea) & raw_tbl$UsableArea>0,
                       c("SampleEventID",
                         "StreamName",
                         "StreamCode",
                         "SiteCode",
                         "Latitude",
                         "Longitude",
                         "SampleEventType",
                         "SampleDate")]

SampleEventID<-distinct(SampleEventID)

SampleEventID<-SampleEventID[SampleEventID$SampleEventType=="(Electro) Fish Sampling",]
SampleEventID<-SampleEventID[!is.na(SampleEventID$Latitude) & !is.na(SampleEventID$Longitude),]

# Setup Fish table --------------------------------------------------------

sub_tbl<-raw_tbl[raw_tbl$SampleEventID %in% SampleEventID$SampleEventID,
                 c("SampleEventID","SampleDate","SpeciesCode","CommonName","ScientificName","NumberOfFishPer100m2","TotalWeightPer100m2")] 

# sub_tbl$EstimatedBiomass<-sub_tbl$BulkWeight/sub_tbl$UsableArea*100
# sub_tbl$NumberOfFish<-sub_tbl$NumberOfFish/sub_tbl$UsableArea*100
sub_tbl$EstimatedBiomass<-sub_tbl$TotalWeightPer100m2
sub_tbl$NumberOfFish<-sub_tbl$NumberOfFishPer100m2

sub_tbl<-sub_tbl[sub_tbl$NumberOfFish>0,] %>% 
  group_by(SampleEventID, SampleDate, SpeciesCode,CommonName,ScientificName) %>% 
  summarise(
    NumberOfFish=sum(NumberOfFish,na.rm=T),
    EstimatedBiomass=sum(EstimatedBiomass,na.rm=T)
  ) %>% 
  ungroup() %>% 
  na.omit()

atr_tbl<-left_join(sub_tbl,lookup_tbl,by="SpeciesCode") 



# Taxa Tables -------------------------------------------------------------

taxa_tbl1 <- atr_tbl %>% 
  group_by(SampleEventID,SampleDate) %>% 
  mutate(Comm_Biomass=sum(EstimatedBiomass,na.rm=T),
         Comm_Abundance=sum(NumberOfFish,na.rm=T),
         Perc_Biomass=EstimatedBiomass/Comm_Biomass,
         Perc_Abundance=NumberOfFish/Comm_Abundance
  ) %>% 
  ungroup() %>% 
  select(SampleEventID,SampleDate,SpeciesCode,Comm_Biomass,Comm_Abundance,Perc_Biomass,Perc_Abundance) %>% 
  mutate(SpeciesCode=factor(SpeciesCode)) %>% 
  group_by(SampleEventID,SampleDate) %>% 
  complete(SpeciesCode,fill = list(Comm_Biomass=0,Comm_Abundance=0,Perc_Biomass=0,Perc_Abundance=0)) %>% 
  ungroup() %>% 
  mutate(Comm_Biomass=ifelse(Perc_Abundance>0 & Comm_Biomass==0,NA_real_,Comm_Biomass)) %>% 
  mutate(Perc_Biomass=ifelse(Perc_Abundance>0 & Perc_Biomass==0,NA_real_,Perc_Biomass))


# No Catch Table ----------------------------------------------------------

no_catch_tbl<-SampleEventID %>% 
  as_tibble() %>% 
  select(SampleEventID,SampleDate) %>% 
  left_join(taxa_tbl1,by = c("SampleEventID", "SampleDate")) %>% 
  filter(is.na(Comm_Biomass)) %>% 
  mutate_at(vars(Comm_Biomass:Perc_Abundance),~0) %>% 
  mutate(SpeciesCode=atr_tbl$SpeciesCode[[1]]) %>% 
  mutate(SpeciesCode=factor(SpeciesCode,levels=unique(taxa_tbl1$SpeciesCode))) %>% 
  group_by(SampleEventID,SampleDate) %>% 
  complete(SpeciesCode,fill = list(Comm_Biomass=0,Comm_Abundance=0,Perc_Biomass=0,Perc_Abundance=0)) %>% 
  ungroup() 

lookup_tbl2<-read_csv(file.path("data","fish_lookup.csv")) %>% 
  mutate(`name to use`=trimws(`name to use`))
colnames(lookup_tbl2)[colnames(lookup_tbl2)=="OMNR Code"]<-"SpeciesCode"

taxa_tbl<-taxa_tbl1 %>% 
  bind_rows(no_catch_tbl) %>% 
  mutate(SpeciesCode=as.character(SpeciesCode)) %>% 
  mutate(SpeciesCode=as.integer(SpeciesCode)) %>% 
  left_join(lookup_tbl2,by="SpeciesCode") %>% 
  group_by(SampleEventID,SampleDate,`name to use`) %>% 
  reframe(across(where(is.numeric),~sum(.,na.rm=T)),
          across(where(is.character),~head(.[!is.na(.)],1))
  ) %>% 
  ungroup() %>% 
  select(any_of(colnames(lookup_tbl2)),everything())

# Calculate endpoints -----------------------------------------------------

atr_tbl<-left_join(sub_tbl,lookup_tbl,by="SpeciesCode") 

ep_tbl<-calc_fish_ep(atr_tbl)

# No Catch Table ----------------------------------------------------------

no_catch_tbl<-SampleEventID %>% 
  select(SampleEventID,SampleDate) %>% 
  left_join(ep_tbl,by = c("SampleEventID", "SampleDate")) %>% 
  filter(is.na(Comm_Richness)) %>% 
  mutate_at(vars(Comm_Biomass:Rep_Lith_Rich),~0)

ep_tbl[sapply(ep_tbl,is.nan)]<-NA

ep_tbl<-ep_tbl %>% 
  bind_rows(no_catch_tbl)


# Endpoint Index ----------------------------------------------------------
ep_tbl_lng<-ep_tbl %>% 
  gather("Endpoint","Value",-SampleEventID,-SampleDate) %>% 
  mutate(Value=ifelse(is.nan(Value),NA,Value)) %>% 
  mutate(Endpoint_group=case_when(
    grepl("^Therm_",Endpoint) ~ "Thermal Regime",
    grepl("^Troph_",Endpoint) ~ "Trophic Guild",
    grepl("^Tol_",Endpoint) ~ "Tolerance Group",
    grepl("^Spec_",Endpoint) ~ "Individual Species",
    grepl("^Env_",Endpoint) ~ "Environment Preference",
    grepl("^Rep_",Endpoint) ~ "Reproductive Guild",
    grepl("^Comm_",Endpoint) ~ "General Community"
  )) %>%
  mutate(Endpoint_subgroup=str_split(Endpoint,"_",simplify = T)[,2]) %>% 
  mutate(Endpoint_subgroup=case_when(
    Endpoint_subgroup == "SATI" ~ "SATI",
    Endpoint_subgroup == "cold" ~ "Cold water",
    Endpoint_subgroup == "cool" ~ "Cool water",
    Endpoint_subgroup == "coolcold" ~ "Cold-Cool water",
    Endpoint_subgroup == "warm" ~ "Warm water",
    Endpoint_subgroup == "invert" ~ "Invertivore",
    Endpoint_subgroup == "detrit" ~ "Detritivore",
    Endpoint_subgroup == "carn" ~ "Carniivore",
    Endpoint_subgroup == "herb" ~ "Herbiivore",
    Endpoint_subgroup == "plank" ~ "Planktivore",
    Endpoint_subgroup == "tol" ~ "Tolerant",
    Endpoint_subgroup == "intol" ~ "Intolerant",
    Endpoint_subgroup == "RBD" ~ "Rainbow Darter",
    Endpoint_subgroup == "BKT" ~ "Brook Trout",
    Endpoint_subgroup == "GDF" ~ "Goldfish",
    Endpoint_subgroup == "RDG" ~ "Round Goby",
    Endpoint_subgroup == "CKC" ~ "Creek Chub",
    Endpoint_subgroup == "CMC" ~ "Common Carp",
    Endpoint_subgroup == "SKP" ~ "Mottled and Slimy Sculpin",
    Endpoint_subgroup == "LMP" ~ "Lampreys",
    Endpoint_subgroup == "DRT" ~ "Sensitive Darters",
    Endpoint_subgroup == "CEB" ~ "Centrarchids and Basses",
    Endpoint_subgroup == "CTF" ~ "Catfishes",
    Endpoint_subgroup == "SAL" ~ "Salmonids",
    Endpoint_subgroup == "benthic" ~ "Benthic",
    Endpoint_subgroup == "pelagic" ~ "Pelagic",
    Endpoint_subgroup == "benthopelagic" ~ "Benthopelagic",
    Endpoint_subgroup == "guard" ~ "Guarders",
    Endpoint_subgroup == "BrHid" ~ "Brood Hiders/Nest Spawners",
    Endpoint_subgroup == "OSS" ~ "Open Substratum/Substratum Choosers",
    Endpoint_subgroup == "Lith" ~ "Lithophilic Spawners",
    Endpoint=="Comm_Biomass" ~ "Total Biomass",
    Endpoint=="Comm_Abundance" ~ "Total Abundance",
    Endpoint=="Comm_Richness" ~ "Total Richness"
  )) %>% 
  mutate(Endpoint_type=case_when(
    grepl("_Bioperc$",Endpoint) ~ "Biomass (%)",
    grepl("_Abuperc$",Endpoint) ~ "Abundance (%)",
    grepl("_Bio$",Endpoint) ~ "Total Biomass",
    grepl("_Abu$",Endpoint) ~ "Total Abundance",
    grepl("_Rich$",Endpoint) ~ "Richness",
    Endpoint=="Comm_Biomass" ~ "Total Biomass",
    Endpoint=="Comm_Abundance" ~ "Total Abundance",
    Endpoint=="Comm_Richness" ~ "Richness"
  )) %>% 
  filter(!is.na(Value))


# Write results to excel --------------------------------------------------

ep_tbl_out<-ep_tbl %>% 
  left_join(SampleEventID) %>% 
  select(StreamName, StreamCode, SiteCode, Latitude, Longitude, SampleEventType, SampleDate,SampleEventID,everything())

ep_tbl_lng_out<-ep_tbl_lng %>% 
  left_join(SampleEventID) %>% 
  select(StreamName, StreamCode, SiteCode, Latitude, Longitude, SampleEventType, SampleDate,SampleEventID,everything())

param_tbl_out<-ep_tbl_lng %>% 
  select(Endpoint_group,Endpoint_subgroup,Endpoint_type,Endpoint) %>% 
  distinct()

taxa_tbl_out<-taxa_tbl %>% 
  left_join(SampleEventID) %>% 
  select(StreamName, StreamCode, SiteCode, Latitude, Longitude, SampleEventType, SampleDate,SampleEventID,everything())


wb = createWorkbook()

addWorksheet(wb, "Endpoints")
addWorksheet(wb, "Results - Wide")
addWorksheet(wb, "Results - Long")
addWorksheet(wb, "Taxa Table")

writeData(wb,param_tbl_out, sheet="Endpoints", rowNames =FALSE)
writeData(wb,ep_tbl_out, sheet="Results - Wide", rowNames =FALSE)
writeData(wb,ep_tbl_lng_out, sheet="Results - Long", rowNames =FALSE)
writeData(wb,taxa_tbl_out, sheet="Taxa Table", rowNames =FALSE)

saveWorkbook(wb, file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),overwrite = T)


# Generate Point Shapefile ------------------------------------------------
library(sf)
library(tidyverse)

fish_ep<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Results - Wide")

pnts<-fish_ep %>%
  mutate(SampleDate=as.Date(SampleDate)) %>%
  #filter(SampleDate>=as.Date("2000-01-01")) %>%
  select(StreamName:SampleEventID) %>%
  distinct() %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=st_crs(4326),remove = F) %>%
  st_transform(st_crs(3161)) 


saveRDS(pnts,file.path("data","Processed","SamplePoint","Fish_points.rds"))
write_sf(pnts,file.path("data","Processed","SamplePoint","Fish_points.shp"))

# BIC Endpoints -----------------------------------------------------------


