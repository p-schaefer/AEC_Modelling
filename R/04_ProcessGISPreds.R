library(tidyverse)
library(sf)
library(terra)
library(whitebox)
library(future.apply)
library(ihydro)

#HW_save_loc<-"/mnt/storage/HW"
HW_save_loc<-file.path("data","HW")
if (!dir.exists(HW_save_loc)) dir.create(HW_save_loc)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

n_cores<-availableCores(logical = F)-1

plan(multisession(workers=2))

# AEC region processing ---------------------------------------------------

aec_region<-lapply(lu_master$stream_packages,function(x) file.path("data","raw","GIS",x))
names(aec_region)<-sapply(names(aec_region),function(x) file.path("data","raw","GIS",x))

# extract IH data
IH_regions<-lapply(names(aec_region),function(aes_nm){
  
  IH_nm<-aec_region[[aes_nm]]
  
  enf_dem<-vrt(map_chr(IH_nm,~(file.path("/vsizip",.,gsub(".zip","",gsub("OIH-Data-Package-","IntegratedHydrology",basename(.))),"EnforcedDEM.tif")))) #%>% 
  
  IH_list<-list(enf_dem=enf_dem)
  
  return(IH_list)
})
names(IH_regions)<-gsub(".zip","",basename(names(aec_region)))

# Prepare AEC subregions data
sub_regions<-future_lapply(names(aec_region),function(aes_nm){
  unzip(aes_nm,exdir=td)
  
  zip_cont<-list.files(file.path(td,gsub(".zip","",basename(aes_nm))),recursive = T,full.names = T)
  sub_regions<-zip_cont[grepl("/w",zip_cont) & grepl("AEC_Class.lyrx",zip_cont)]
  sub_regions<-gsub("_Class\\.lyrx","_Core\\.gdb",sub_regions)
  names(sub_regions)<-sub_regions
  
  sub_regions<-lapply(sub_regions, st_layers)
  
  sub_region_out<-map2(names(sub_regions),sub_regions,function(src,lyr) {
    list(boundary=read_sf(src,lyr$name[grepl("Boundary",lyr$name)]))
  })
  
  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))


lu_process_kit<-map(setNames(names(sub_regions),names(sub_regions)),function(sub_nm){
  dem<-IH_regions[[sub_nm]]$enf_dem
  
  map(setNames(names(sub_regions[[sub_nm]]),names(sub_regions[[sub_nm]])),function(sub_nm2){
    clip_region<-sub_regions[[sub_nm]][[sub_nm2]]$boundary
    
    if (file.exists(file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,"_loi.gpkg")))) return(NULL)
    
    tp<-tempfile()
    dir.create(tp)
    
    
    dem_sub<-hydroweight::process_input(
      input = terra::rast(
        file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,".gpkg")),
        "dem_final"
      ),
      input_name = "dem",
      variable_names = NULL,
      target = NULL,
      #clip_region = clip_region,
      resample_type ="bilinear",
      working_dir = NULL
    )
    
    lu_sub<-terra::project(terra::rast(lu_master$GIS_Pred$landcover),
                           dem_sub,
                           method="near",
                           mask=T)
    
    lu_sub<-terra::crop(lu_sub,clip_region)
    
    lu_broad<-terra::classify(lu_sub,
                              lu_master$GIS_Pred$landcover_lu %>% 
                                select(Code,LabelG2_code) %>% 
                                distinct() %>% 
                                as.matrix(),
                              other=NA)
    
    lu_narrow<-terra::classify(lu_sub,
                               lu_master$GIS_Pred$landcover_lu %>% 
                                 select(Code,LabelG1_code) %>% 
                                 distinct() %>% 
                                 as.matrix(),
                               other=NA)
    
    names(lu_broad)<-"lu_broad"
    names(lu_narrow)<-"lu_narrow"
    
    broad_names<-lu_master$GIS_Pred$landcover_lu %>% 
      select(LabelG2,LabelG2_code) %>% 
      distinct() %>% 
      mutate(new_name=paste0("br_",LabelG2),
             old_name=paste0("lu_broad_",LabelG2_code))
    
    narrow_names<-lu_master$GIS_Pred$landcover_lu %>% 
      select(LabelG1,LabelG1_code) %>% 
      distinct() %>% 
      mutate(new_name=paste0("nr_",LabelG1),
             old_name=paste0("lu_narrow_",LabelG1_code))
    
    lu_broad<-hydroweight::process_input(
      input = lu_broad,
      input_name = "lu_broad",
      variable_names = NULL,
      target = dem_sub,
      clip_region = clip_region,
      resample_type ="near",
      working_dir = tp
    )
    
    lu_narrow<-hydroweight::process_input(
      input = lu_narrow,
      input_name = "lu_narrow",
      variable_names = NULL,
      target = NULL,
      clip_region = NULL,
      resample_type ="near",
      working_dir = tp
    )
    
    names(lu_broad)<-broad_names$new_name[match(names(lu_broad),broad_names$old_name)]
    names(lu_narrow)<-narrow_names$new_name[match(names(lu_narrow),narrow_names$old_name)]
    
    #browser()
    
    lu_narrow<-terra::subset(lu_narrow, which(names(lu_narrow)!="NA"))
    lu_broad<-terra::subset(lu_broad, which(names(lu_broad)!="NA"))
    
    lu_broad<-writeRaster(lu_broad,file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_broad.tif"))),overwrite=T)
    lu_narrow<-writeRaster(lu_narrow,file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_narrow.tif"))),overwrite=T)
    
    
    
    loi_oit<-ihydro::process_loi(
      input = NULL,
      dem = terra::rast(
        file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,".gpkg")),
        "dem_final"
      ),
      clip_region = NULL,
      num_inputs = list(),
      cat_inputs = list(lu_broad=file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_broad.tif"))),
                        lu_narrow=file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_narrow.tif")))),
      output_filename = file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,"_loi.gpkg")),
      variable_names = NULL,
      temp_dir = tp
    )
    
    try(unlink(tp,recursive=T,force=T))
    return(NULL)
    #for (i in out) terra::writeRaster(i,file.path("data","Processed","GIS",paste0(sub_nm2,names(i),".tif")))
    
  })
  
})


# Add LDI -----------------------------------------------------------------

lu_process_kit<-map(setNames(names(sub_regions),names(sub_regions)),function(sub_nm){
  dem<-IH_regions[[sub_nm]]$enf_dem
  
  map(setNames(names(sub_regions[[sub_nm]]),names(sub_regions[[sub_nm]])),function(sub_nm2){
    #browser()
    clip_region<-sub_regions[[sub_nm]][[sub_nm2]]$boundary
    
    if (file.exists(file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,"_LDI_loi.gpkg")))) return(NULL)
    
    tp<-tempfile()
    dir.create(tp)
    
    
    dem_sub<-hydroweight::process_input(
      input = terra::rast(
        file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,".gpkg")),
        "dem_final"
      ),
      input_name = "dem",
      variable_names = NULL,
      target = NULL,
      #clip_region = clip_region,
      resample_type ="bilinear",
      working_dir = NULL
    )
    
    lu_sub<-terra::project(terra::rast(lu_master$GIS_Pred$landcover),
                           dem_sub,
                           method="near",
                           mask=T)
    
    lu_sub<-terra::crop(lu_sub,clip_region)
    
    lu_LDI<-terra::classify(lu_sub,
                            lu_master$GIS_Pred$landcover_lu %>% 
                              select(Code,LDI) %>% 
                              distinct() %>% 
                              as.matrix(),
                            others=NA)
    
    lu_LDI2<-terra::classify(lu_sub,
                             lu_master$GIS_Pred$landcover_lu %>% 
                               select(Code,LDI_Natural) %>% 
                               distinct() %>% 
                               as.matrix(),
                             others=NA)
    
    
    names(lu_LDI)<-"LDI"
    names(lu_LDI2)<-"LDI_Natural"
    
    
    lu_LDI<-hydroweight::process_input(
      input = lu_LDI,
      input_name = "LDI",
      variable_names = NULL,
      target = dem_sub,
      clip_region = clip_region,
      resample_type ="bilinear",
      working_dir = tp
    )
    lu_LDI2<-hydroweight::process_input(
      input = lu_LDI2,
      input_name = "LDI_Natural",
      variable_names = NULL,
      target = dem_sub,
      clip_region = clip_region,
      resample_type ="bilinear",
      working_dir = tp
    )
    
    #browser()
    
    lu_LDI<-terra::subset(lu_LDI, which(names(lu_LDI)!="NA"))
    lu_LDI2<-terra::subset(lu_LDI2, which(names(lu_LDI2)!="NA"))
    
    lu_LDI<-writeRaster(lu_LDI,file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_LDI.tif"))),overwrite=T)
    lu_LDI2<-writeRaster(lu_LDI2,file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_LDI_Natural.tif"))),overwrite=T)
    
    
    
    loi_oit<-ihydro::process_loi(
      input = NULL,
      dem = terra::rast(
        file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,".gpkg")),
        "dem_final"
      ),
      clip_region = NULL,
      num_inputs = list(
        LDI=file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_LDI.tif"))),
        LDI_Natural=file.path(tp,paste0(paste0(clip_region$WorkUnitName,"_LDI_Natural.tif")))
      ),
      cat_inputs = list(),
      output_filename = file.path("data","Processed","ihydro",paste0(clip_region$WorkUnitName,"_LDI_loi.gpkg")),
      variable_names = NULL,
      temp_dir = tp
    )
    
    try(unlink(tp,recursive=T,force=T))
    return(NULL)
    #for (i in out) terra::writeRaster(i,file.path("data","Processed","GIS",paste0(sub_nm2,names(i),".tif")))
    
  })
  
})


plan(sequential)

