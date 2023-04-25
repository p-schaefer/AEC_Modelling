# This script will be used to generate/update lookup tables
library(tidyverse)

master_lu<-list()


# AEC Stream networks with associated IH ----------------------------------
AEC_fl<-list.files(file.path("data","raw","GIS"),pattern = "AEC_Core")
IH_fl<-list.files(file.path("data","raw","GIS"),pattern = "OIH-Data-Package")

# This pairs the associated AEC with OIH packages
stream_packages<-as.list(setNames(AEC_fl,AEC_fl))
stream_packages[grepl("LakeOntario",names(stream_packages))] <- list(IH_fl[grepl("SW",IH_fl)])
stream_packages[grepl("LakeErie",names(stream_packages))] <- list(IH_fl[grepl("SW",IH_fl)])
stream_packages[grepl("LakeHuronSouth",names(stream_packages))] <- list(IH_fl[grepl("SW|NE",IH_fl)])
stream_packages[grepl("OttawaStLawrenceRivers",names(stream_packages))] <- list(IH_fl[grepl("SE|NE",IH_fl)])
stream_packages[grepl("LakeHuronNorth",names(stream_packages))] <- list(IH_fl[grepl("NE",IH_fl)])
stream_packages[grepl("LakeSuperior",names(stream_packages))] <- list(IH_fl[grepl("NC",IH_fl)])

master_lu$stream_packages<-stream_packages


# GIS Predictor Data ------------------------------------------------------
# This is a table to standardize and simplify landcover layers
GIS_Pred<-list(landcover=file.path("/vsizip","data","raw","GIS","OntarioLandCoverComp-v2.zip","OntarioLandCoverComp-v2","OLCC_V2_TIFF","OLCC_V2_TIFF.tif"),
               landcover_lu=read_csv(file.path("data","LC_lookup.csv"),
                                     show_col_types = FALSE) %>% 
                 mutate(LDI=LDI*100,
                        LDI_Natural=LDI_Natural*100) %>% 
                 filter(Layer=="OLCC") %>% 
                 mutate(LabelG1_code=as.numeric(factor(LabelG1)),
                        LabelG2_code=as.numeric(factor(LabelG2)))

               )

master_lu$GIS_Pred<-GIS_Pred
# Write Lookup File -------------------------------------------------------

saveRDS(master_lu,file.path("data","lookups.rds"))
