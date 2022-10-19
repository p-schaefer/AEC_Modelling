library(tidyverse)
library(sf)
library(terra)
library(whitebox)
library(future.apply)
library(hydroweight)

#HW_save_loc<-"/mnt/storage/HW"
HW_save_loc<-file.path("data","HW")
if (!dir.exists(HW_save_loc)) dir.create(HW_save_loc)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

n_cores<-parallel::detectCores()

plan(multisession)

# SOLRIS Processing -------------------------------------------------------

crs_master<-rast(file.path("/vsizip",
                           file.path("data","raw","GIS",lu_master$stream_packages$AEC_Core_Package01_LakeErie.zip),
                           gsub(".zip","",gsub("OIH-Data-Package-","IntegratedHydrology",basename(lu_master$stream_packages$AEC_Core_Package01_LakeErie.zip))),
                           "EnforcedDEM.tif")) 

solris<-rast(file.path("/vsizip",file.path("data","raw","GIS",lu_master$GIS_Pred$landcover,"SOLRIS_Version_3_0_LAMBERT.tif")))
solris<-project(solris,crs_master,method="near")

solris_lu<-lu_master$GIS_Pred$landcover_lu 

ldi<-classify(solris,
              solris_lu %>%
                select(Code,LDI) %>%
                as.matrix()
)

lc_class<-classify(solris,
                   solris_lu %>%
                     select(Code,BK_Label_code) %>%
                     as.matrix()
)

lc_out<-list(
  LDI=ldi,
  LC_Class=lc_class
)


# Calculate Weighted Summaries --------------------------------------------

fl<-unzip(file.path("data","HW","HW.zip"),list=T)

