# This script will be used to generate/update lookup tables
library(tidyverse)

master_lu<-list()


# AEC Stream networks with associated IH ----------------------------------
AEC_fl<-list.files(file.path("data","raw","GIS"),pattern = "AEC_Core")
IH_fl<-list.files(file.path("data","raw","GIS"),pattern = "OIH-Data-Package")

stream_packages<-as.list(setNames(AEC_fl,AEC_fl))
stream_packages[grepl("LakeOntario",names(stream_packages))] <- list(IH_fl[grepl("SW",IH_fl)])
stream_packages[grepl("LakeErie",names(stream_packages))] <- list(IH_fl[grepl("SW",IH_fl)])
stream_packages[grepl("LakeHuronSouth",names(stream_packages))] <- list(IH_fl[grepl("SW|NE",IH_fl)])
stream_packages[grepl("OttawaStLawrenceRivers",names(stream_packages))] <- list(IH_fl[grepl("SE|NE",IH_fl)])
stream_packages[grepl("LakeHuronNorth",names(stream_packages))] <- list(IH_fl[grepl("NE",IH_fl)])
stream_packages[grepl("LakeSuperior",names(stream_packages))] <- list(IH_fl[grepl("NC",IH_fl)])

master_lu$stream_packages<-stream_packages


# GIS Predictor Data ------------------------------------------------------

GIS_Pred<-list(landcover=list.files(file.path("data","raw","GIS"),pattern = "SOLRIS"),
               landcover_lu=read_csv(file.path("data","SOLRIS_AAFC_Priority.csv"),
                                     show_col_types = FALSE) %>% 
                 mutate(LDI=LDI*100) %>% 
                 filter(Layer=="SOLRIS") %>% 
                 mutate(BK_Label_code=as.numeric(factor(BK_Label))),
               Ontario_Landcover_compilation=list.files(file.path("data","raw","GIS"),pattern = "OntarioLandCoverComp")
               
               )

master_lu$GIS_Pred<-GIS_Pred
# Write Lookup File -------------------------------------------------------

saveRDS(master_lu,file.path("data","lookups.rds"))
