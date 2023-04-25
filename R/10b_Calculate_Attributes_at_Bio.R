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

bio_points<-read_rds(file.path("data","final","int_results.rds"))

bio_points<-map(bio_points,~.$bio_pnt_out)

attrs<-map_dfr(fl_base,.id="region",function(x){
  print(x)

  bio_sub<-str_split_1(basename(x),"_")[[1]]
  
  bio_sub<-bio_points[grepl(bio_sub,names(bio_points))][[1]] 
  
  if (nrow(bio_sub)==0) return(NULL)
  
  bio_sub<-bio_sub %>% 
    select(link_id,ProvReachID,StreamCode,SiteCode,SampleDate,SampleEventID) %>% 
    filter(!is.na(link_id))
  
  tp<-tempfile()
  dir.create(tp)
  
  #browser()
  
  if (!file.exists(gsub(".gpkg","_biopoint.csv",x))){
    loi_cols<-ihydro_layers(ihydro::as.ihydro(gsub(".gpkg","_loi.gpkg",x)))
    
    loi_cols<-loi_cols %>% 
      filter(data_group=="loi") %>% 
      filter(layer_name!="NA") %>% 
      pull(layer_name)
    
    out<-ihydro::fasttrib_points(
      input=ihydro::as.ihydro(x),
      out_filename=gsub(".gpkg","_biopoint.csv",x),
      loi_file = gsub(".gpkg","_loi.gpkg",x),
      loi_cols = loi_cols,
      iDW_file = gsub(".gpkg","_DW.gpkg",x),
      store_iDW = F,
      sample_points = NULL,
      link_id = unique(bio_sub$link_id),
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
  
  if (!file.exists(gsub(".gpkg","_biopoint_LDI.csv",x))){
    out<-ihydro::fasttrib_points(
      input=ihydro::as.ihydro(x),
      out_filename=gsub(".gpkg","_biopoint_LDI.csv",x),
      loi_file = gsub(".gpkg","_LDI_loi.gpkg",x),
      iDW_file = gsub(".gpkg","_DW.gpkg",x),
      store_iDW = F,
      sample_points = NULL,
      link_id = unique(bio_sub$link_id),
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
  
  out<-bio_sub %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    left_join(
      left_join(read.csv(gsub(".gpkg","_biopoint.csv",x)),
                read.csv(gsub(".gpkg","_biopoint_LDI.csv",x))
      ) %>% 
        select(-status)
    )
  
  return(out)

})

if (anyNA(attrs)) attrs[is.na(attrs)]<-0

write_csv(attrs,file.path("data","final","Biopoint_LC_Attr.csv"))

plan(sequential)
