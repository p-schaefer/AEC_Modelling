#################### Install required packages ####################
#installation of packages require compilation, and may take a lot of time

#---- terra ----#
#if having compilation failure, follow instructions on GitHub: https://github.com/rspatial/terra?tab=readme-ov-file
#for window users: 
  #first install Rcpp package, which terra depends on:
install.packages("Rcpp")
  #second, download manually Rtools, a C++ compiler that R can use
#Rtools: https://cran.r-project.org/bin/windows/Rtools/
  #lastly, install terra directly from GitHub repository
remotes::install_github("rspatial/terra")

#---- sf ----#
#may require Rtools for proper installation.
#if install.packages(sf) does not work, try to install directy from GitHub repository
remotes::install_github("r-spatial/sf") #development version

#---- whitebox ----#
#WhiteboxTools and whitebox are required for ihydro
install.packages("whitebox")
if (F){
  install_whitebox()
  # Possible warning message:
  # ------------------------------------------------------------------------
  # Could not find WhiteboxTools!
  # ------------------------------------------------------------------------
  #
  # Your next step is to download and install the WhiteboxTools binary:
  #     > whitebox::install_whitebox()
  #
  # If you have WhiteboxTools installed already run `wbt_init(exe_path=...)`':
  #    > wbt_init(exe_path='/home/user/path/to/whitebox_tools')
  #
  # For whitebox package documentation, ask for help:
  #    > ??whitebox
  #
  # For more information visit https://giswqs.github.io/whiteboxR/
  #
  # ------------------------------------------------------------------------
}

#---- ihydro ----#
# created by p-schaefer, an integrated hydrology tools for environmental science
#if install.packages(ihydro) does not work, try to install directly from GitHub repository
remotes::install_github("p-schaefer/ihydro")


#----library----#

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

plan(multisession(workers=8))

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
    list(boundary=read_sf(src,lyr$name[grepl("Boundary",lyr$name)]),
         stream=read_sf(src,lyr$name[grepl("Reach",lyr$name)]))
  })
  
  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))

# generate ihydro products
sub_regions_out<-map2(sub_regions,IH_regions,
                      function(aec,ih){
                        map(aec,function(aec_sub){
                          #browser()
                          print(paste0(aec_sub$boundary$WorkUnitName,": ",Sys.time()))
                          
                          temp_path<-tempfile()
                          dir.create(temp_path)
                          
                          if (!file.exists(file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,".gpkg")))) { #return(file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,".gpkg")))
                            
                            #browser()
                            enf_dem<-ih$enf_dem
                            #enf_dem<-rast(ih$enf_dem)
                            
                            proc_dem<-hydroweight::process_input(
                              enf_dem,
                              target=terra::rasterize(aec_sub$boundary %>%
                                                        mutate(field=1) ,
                                                      enf_dem,
                                                      field="field"),
                              clip_region = aec_sub$boundary 
                            )
                            
                            s1<-aec_sub$stream %>%
                              st_transform(st_crs(proc_dem)) %>% 
                              mutate(ProvReachID=factor(ProvReachID)) %>% 
                              mutate(ProvReachID_N=as.numeric(ProvReachID))
                            
                            r1<-proc_dem
                            
                            sr1<-mask(r1,s1 %>% filter(Network_Line_Type =="Shoreline Virtual Connector") %>% st_buffer(units::set_units(30,"m")) %>% vect())
                            
                            r1[!is.na(sr1)]<-sr1-5
                            
                            sr1<-mask(r1,s1 %>% st_buffer(units::set_units(60,"m")) %>% vect())
                            
                            r1[!is.na(sr1)]<-sr1-5
                            
                            sr1<-mask(r1,s1 %>% st_buffer(units::set_units(30,"m")) %>% vect())
                            
                            r1[!is.na(sr1)]<-sr1-5
                            
                            
                            writeRaster(r1,file.path(temp_path,"enf_dem.tif"),overwrite=T)
                            
                            wbt_feature_preserving_smoothing(
                              dem = "enf_dem.tif",
                              output = "smooth_enf_dem.tif",
                              wd =temp_path
                            )
                            
                            #browser()
                            
                            ihydro_out<-process_flowdir(
                              dem=file.path(temp_path,"smooth_enf_dem.tif"),
                              depression_corr="fill",
                              threshold=1000L,  
                              return_products=F,
                              output_filename=file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,".gpkg")),
                              temp_dir=temp_path, 
                              verbose=F
                            )
                            
                            ihydro_out<-generate_vectors(
                              input=ihydro_out,
                              return_products=F,
                              temp_dir=temp_path,
                              verbose=F
                            )
                            
                            ihydro_out<-trace_flowpaths(
                              input=ihydro_out,
                              pwise_dist =F,
                              return_products=F,
                              temp_dir=temp_path,
                              verbose=F
                            )
                            
                            a1<-ihydro::get_catchment(ihydro_out)
                          }
                          
                          if (F & !file.exists(file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,"_DW.gpkg")))){
                            ihydro_out<-prep_weights(
                              input=ihydro::as.ihydro(file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,".gpkg"))),
                              output_filename =file.path("data","Processed","ihydro",paste0(aec_sub$boundary$WorkUnitName,"_DW.gpkg")),
                              target_o_type = "segment_whole",
                              weighting_scheme = c( "iFLS", "HAiFLS", "iFLO", "HAiFLO"),
                              temp_dir=temp_path
                            )
                          }
                          
                          t1<-try(file.remove(list.files(temp_path,recursive = T,full.names=T)),silent=T)
                          
                          t1<-try(unlink(temp_path,recursive = T,force=T),silent=T)
                          
                          
                          return(NULL)
                          
                          
                        })
                      })

plan(sequential)
