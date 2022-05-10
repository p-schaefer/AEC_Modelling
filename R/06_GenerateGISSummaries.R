# Generate GIS summaries
#devtools::install_github("p-schaefer/hydroweight@p-schaefer-patch-1",force=TRUE)

library(tidyverse)
library(sf)
library(terra)
library(whitebox)
library(future.apply)
library(hydroweight)

lu_master<-readRDS(file.path("data","lookups.rds"))

myinv <- function(x) {
  (x * 0.001 + 1)^-1
} ## 0.001 multiplier turns m to km

n_cores<-8

td<-tempdir()


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


# AEC region processing ---------------------------------------------------

aec_region<-lapply(lu_master$stream_packages,function(x) file.path("data","raw","GIS",x))
names(aec_region)<-sapply(names(aec_region),function(x) file.path("data","raw","GIS",x))


# extract IH data
IH_regions<-lapply(names(aec_region),function(aes_nm){
  
  IH_nm<-aec_region[[aes_nm]]
  
  enf_dem<-map(IH_nm,~rast(file.path("/vsizip",.,gsub(".zip","",gsub("OIH-Data-Package-","IntegratedHydrology",basename(.))),"EnforcedDEM.tif"))) %>% 
    sprc() %>% 
    merge()
  
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
    list(boundary=read_sf(src,lyr$name[grepl("Boundary",lyr$name)]),
         stream=read_sf(src,lyr$name[grepl("Reach",lyr$name)]))
  })
  
  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))


# Process subregion hydrology data
sub_region_out<-lapply(setNames(names(sub_regions),names(sub_regions)),function(reg_nm){
  sub_all<-sub_regions[[reg_nm]]
  IH_list<-c(IH_regions[[reg_nm]],
             lc_out
  )
  
  lapply(setNames(names(sub_all),names(sub_all)),function(sub_nm){
    temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
    sub_r<-sub_all[[sub_nm]]
    
    message(paste("Processing:",sub_nm))
    suppressWarnings(dir.create(temp_path))
    
    IH_list_temp<-map(IH_list,~crop(.,vect(sub_r$boundary),mask=T))
    w1<-map2(IH_list_temp,names(IH_list_temp), ~writeRaster(.x,file.path(temp_path,paste0(.y,".tif")),overwrite=T))
    write_sf(sub_r$stream %>% st_transform(st_crs(IH_list_temp[[1]])),file.path(temp_path,"strm.shp"))
    
    s1<-sub_r$stream %>%
      st_transform(st_crs(IH_list_temp[[1]])) %>% 
      mutate(ProvReachID=factor(ProvReachID)) %>% 
      mutate(ProvReachID_N=as.numeric(ProvReachID))
    
    r1<-IH_list_temp$enf_dem
    
    sr1<-mask(r1,s1 %>% filter(Network_Line_Type =="Shoreline Virtual Connector") %>% st_buffer(units::set_units(15,"m")) %>% vect())
    
    r1[!is.na(sr1)]<-sr1-5
    
    sr1<-mask(r1,s1 %>% st_buffer(units::set_units(60,"m")) %>% vect())
    
    r1[!is.na(sr1)]<-sr1-5
    
    sr1<-mask(r1,s1 %>% st_buffer(units::set_units(30,"m")) %>% vect())
    
    r1[!is.na(sr1)]<-sr1-5
    
    writeRaster(r1,file.path(temp_path,"enf_dem.tif"),overwrite=T)
    
    wbt_feature_preserving_smoothing(
      dem = "enf_dem.tif",
      output = "fill_enf_dem.tif",
      wd =temp_path
    )
    
    wbt_fill_depressions(
      dem = "enf_dem.tif",
      output = "fill_enf_dem.tif",
      wd =temp_path
    )
    
    wbt_d8_pointer(
      dem="fill_enf_dem.tif",
      output="enf_d8.tif",
      wd =temp_path
    )
    
    wbt_d8_flow_accumulation(
      input="enf_d8.tif",
      output="flow_accu.tif",
      pntr = T,
      wd =temp_path
    )
    
    wbt_extract_streams(
      flow_accum ="flow_accu.tif",
      output="strm_grid.tif",
      threshold = 1000,
      wd =temp_path
    )
    
    wbt_stream_link_class(
      wd=temp_path,
      esri_pntr=F,
      d8_pntr="enf_d8.tif",
      streams = "strm_grid.tif",
      output= "strm_link_class.tif"
    )
    
    wbt_farthest_channel_head(
      wd=temp_path,
      d8_pntr="enf_d8.tif",
      streams = "strm_grid.tif",
      output= "strm_link_FrUSchan.tif"
    )
    
    out<-c(
      IH_list_temp,
      sub_r,
      list(
        clean_stream_line=s1
      )
    )
    return(out)
  })
}) 


# Identify Pour Points
sub_region_PourPoint<-lapply(setNames(names(sub_region_out),names(sub_region_out)),function(reg_nm){
  sub_all<-sub_region_out[[reg_nm]]
  
  lapply(setNames(names(sub_all),names(sub_all)),function(sub_nm){
    message(paste("Processing:",sub_nm))
    
    temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
    sub_r<-sub_all[[sub_nm]]
    
    # Identify pour points for each stream segment
    id1<-sub_r$clean_stream_line %>% 
      st_buffer(units::set_units(30,"m")) %>% 
      vect() %>% 
      rasterize(y=sub_all[[sub_nm]]$enf_dem,
                field="ProvReachID_N")
    
    id2<-as.points(id1)
    
    attr<-c(
      rast(file.path(temp_path,"strm_link_class.tif")),
      rast(file.path(temp_path,"strm_link_FrUSchan.tif"))
    )
    
    id3<-suppressMessages(terra::extract(attr,id2))
    
    id4<-id2 %>% 
      st_as_sf() %>% 
      bind_cols(id3) %>% 
      filter(strm_link_class==1 | strm_link_class==2) %>% 
      mutate(x=st_coordinates(.)[,1],
             y=st_coordinates(.)[,2]) %>% 
      group_by(ProvReachID_N) %>% 
      mutate(x=x[strm_link_FrUSchan==max(strm_link_FrUSchan,na.rm=T)][1],
             y=y[strm_link_FrUSchan==max(strm_link_FrUSchan,na.rm=T)][1]) %>% 
      #mutate(strm_link_FrUSchan=max(strm_link_FrUSchan)) %>% 
      filter(strm_link_FrUSchan==max(strm_link_FrUSchan,na.rm=T)) %>% 
      summarise_all(head,1) %>% 
      ungroup() %>% 
      select(x,y)
    
    s2<-sub_r$clean_stream_line %>% 
      filter(Network_Line_Type !="Shoreline Virtual Connector") %>% 
      st_buffer(units::set_units(29,"m")) %>% 
      st_join(id4,largest =T,join=nngeo::st_nn,maxdist=30)
    
    pp1<-s2 %>% 
      tibble() %>% 
      select(ProvReachID,x,y) %>% 
      filter(!is.na(x)) %>% 
      mutate(ProvReachID=as.character(ProvReachID)) %>% 
      st_as_sf(coords=c("x","y")) 
    
    st_crs(pp1)<-st_crs(s2)
    
    return(pp1)
    
  })
  
})

# Generate Hydroweights
plan(list(tweak(multisession, workers = 2), tweak(multisession, workers = 8)))

fl<-list.files(td,"AEC_Core$")
names(fl)<-fl
HW_out<-future_lapply(fl,function(sub_nm){
  temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
  
  loc<-sapply(sub_region_PourPoint,function(x) names(x)==sub_nm)
  
  pp_all<-sub_region_PourPoint[[names(sub_region_PourPoint)[sapply(loc,any)]]][[sub_nm]]
  
  #pp_all<-head(pp_all)#,round(length(pp1_split)*0.01)
  
  pp1_split<-split(pp_all,pp_all$ProvReachID)
  
  
  future_lapply(pp1_split,function(pp_sub){

    out_file<-file.path(file.path("data","HW",paste0(pp_sub$ProvReachID,"_inv_distances.rds")))
    
    if (file.exists(out_file)) return(T)
    
    write_sf(pp_sub,file.path(temp_path,paste0(pp_sub$ProvReachID,"-temp_pnt.shp")))
    
    wbt_watershed(
      wd=temp_path,
      d8_pntr = "enf_d8.tif",
      pour_pts = paste0(pp_sub$ProvReachID,"-temp_pnt.shp"),
      output = paste0(pp_sub$ProvReachID,"-temp_catch.tif")
    )
    
    rcat<-st_as_sf(rast(file.path(temp_path,paste0(pp_sub$ProvReachID,"-temp_catch.tif"))) %>% as.polygons())
    
    hw1 <- try(suppressMessages(hydroweight::hydroweight(
      hydroweight_dir = temp_path,
      target_O = pp_sub,
      target_S = "strm_grid.tif",
      target_uid = pp_sub$ProvReachID,
      clip_region = rcat,
      OS_combine = F,
      dem = "fill_enf_dem.tif",
      flow_accum = "flow_accu.tif",
      weighting_scheme = c(
        "lumped", "iFLO", "HAiFLO",
        "iFLS", "HAiFLS"
      ),
      inv_function = myinv
    )),silent=T)
    
    if (inherits(res, "try-error")) return(F)
    
    fc<-file.copy(file.path(temp_path,paste0(pp_sub$ProvReachID,"_inv_distances.rds")),
                  out_file,
                  overwrite = T
    )
    
    shp_fls<-list.files(temp_path,paste0(pp_sub$ProvReachID,"-temp_pnt"),full.names = T)
    
    t1<-file.remove(file.path(temp_path,paste0(pp_sub$ProvReachID,"-temp_catch.tif")),force = F)
    t1<-file.remove(file.path(temp_path,paste0(pp_sub$ProvReachID,"_inv_distances.rds")),force = F)
    t1<-file.remove(shp_fls,force = F)
    
    return(fc)
  })
})




fl<-list.files(file.path(file.path("data","HW"),"_inv_distances.rds"))

hwa_numeric <- suppressMessages(hydroweight_attributes(
  loi = raster::raster(ldi),
  loi_attr_col = "LDI",
  loi_numeric = TRUE,
  loi_numeric_stats = c("distwtd_mean", "distwtd_sd", "mean", "sd", "median", "min", "max"),
  roi = rcat,
  roi_uid = pp_sub$ProvReachID,
  roi_uid_col = "ProvReachID",
  distance_weights = hw1,
  remove_region = NULL,
  return_products = FALSE
))

hwa_categorical <- suppressMessages(hydroweight_attributes(
  loi = raster::raster(lc_class),
  loi_attr_col = "LC_Class",
  loi_numeric = FALSE,
  roi = rcat,
  roi_uid = pp_sub$ProvReachID,
  roi_uid_col = "ProvReachID",
  distance_weights = hw1,
  remove_region = NULL,
  return_products = FALSE
))

hw2<-list()
hw2$LDI<-hwa_numeric
hw2$LC_Class<-hwa_categorical
return(hw2)

})

})

})




HW_out<-map_dfr(hw,~.$LDI) %>% #this wont work as is, need to change LC_Class names back to real names
  left_join(
    map_dfr(hw,~.$LC_Class)
  )

out2<-s1 %>% 
  tibble() %>% 
  left_join(out)

return(out2)


#browser()

#   return(sub_region_out)
#   
# })

plan(sequential)


