library(tidyverse)
library(furrr)
library(sf)
library(terra)
library(whitebox)
library(future.apply)
library(ihydro)

lu_master<-readRDS(file.path("data","lookups.rds"))

plan(multisession(workers=8))

fl<-list.files(file.path("data","Processed","ihydro"),full.names = T)

fl_base<-fl[!grepl("_loi|_DW|.csv|.ini|old",fl)]

names(fl_base)<-gsub(".gpkg","",basename(fl_base))

attrs<-map_dfr(fl_base,.id="region",function(x){
  print(x)
  
  tp<-tempfile()
  dir.create(tp)
  
  #browser()
  
  if (!file.exists(gsub(".gpkg",".csv",x))){
    loi_cols<-ihydro_layers(ihydro::as.ihydro(gsub(".gpkg","_loi.gpkg",x)))
    
    loi_cols<-loi_cols %>% 
      filter(data_group=="loi") %>% 
      filter(layer_name!="NA") %>% 
      pull(layer_name)
    
    out<-ihydro::fasttrib_points(
      input=ihydro::as.ihydro(x),
      out_filename=gsub(".gpkg",".csv",x),
      loi_file = gsub(".gpkg","_loi.gpkg",x),
      loi_cols = loi_cols,
      iDW_file = gsub(".gpkg","_DW.gpkg",x),
      store_iDW = F,
      sample_points = NULL,
      link_id = NULL,
      target_o_type = c("point"),
      weighting_scheme = c("lumped", "iFLS", "HAiFLS", "iFLO", "HAiFLO"),
      loi_numeric_stats = c("mean", "sd", "median", "min", "max"),
      inv_function = function(x) {
        (x * 0.001 + 1)^-1
      },
      temp_dir = tp,
      verbose = T
    )
  }
  
  if (!file.exists(gsub(".gpkg","_LDI.csv",x))){
    out<-ihydro::fasttrib_points(
      input=ihydro::as.ihydro(x),
      out_filename=gsub(".gpkg","_LDI.csv",x),
      loi_file = gsub(".gpkg","_LDI_loi.gpkg",x),
      iDW_file = gsub(".gpkg","_DW.gpkg",x),
      store_iDW = F,
      sample_points = NULL,
      link_id = NULL,
      target_o_type = c("point"),
      weighting_scheme = c("lumped", "iFLS", "HAiFLS", "iFLO", "HAiFLO"),
      loi_numeric_stats = c("mean", "sd", "median", "min", "max"),
      inv_function = function(x) {
        (x * 0.001 + 1)^-1
      },
      temp_dir = tp,
      verbose = T
    )
  }
  
  
  try(unlink(tp,recursive=T,force=T))
  return(left_join(read.csv(gsub(".gpkg",".csv",x)),
                   read.csv(gsub(".gpkg","_LDI.csv",x))
                   ))

})

attrs[is.na(attrs)]<-0

write.csv(attrs,file.path("data","final","All_LC_Attr.csv"))

plan(sequential)
