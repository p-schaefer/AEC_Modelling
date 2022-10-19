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
IH_regions<-future_lapply(names(aec_region),function(aes_nm){
  
  IH_nm<-aec_region[[aes_nm]]
  
  enf_dem<-map(IH_nm,~rast(file.path("/vsizip",.,gsub(".zip","",gsub("OIH-Data-Package-","IntegratedHydrology",basename(.))),"EnforcedDEM.tif"))) %>% 
    sprc() %>% 
    merge() %>% 
    wrap()
  
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

# aec_sub<-sub_regions[[1]][[1]]
# ih<-IH_regions[[1]]

# f1<-file.path("data","Processed","Hydrology",paste0("w01_Lake_Erie_West",".zip"))
# 
# hydro_out<-generate_vectors(
#   input=list(outfile=f1),
#   return_products=T,
#   temp_dir=NULL,
#   verbose=F
# ) 

sub_regions_out<-map2(sub_regions,IH_regions,
                      function(aec,ih){
                        map(aec,function(aec_sub){
                          if (file.exists(file.path("data","Processed","Hydrology",paste0(aec_sub$boundary$WorkUnitName,".zip")))) return(file.path("data","Processed","Hydrology",paste0(aec_sub$boundary$WorkUnitName,".zip")))
                          #browser()
                          print(paste0(aec_sub$boundary$WorkUnitName,": ",Sys.time()))
                          
                          enf_dem<-rast(ih$enf_dem)
                          
                          proc_dem<-hydroweight::process_input(
                            enf_dem,
                            target=terra::rasterize(aec_sub$boundary %>%
                                                      mutate(field=1) %>% 
                                                      st_buffer(units::set_units(30,"m")),
                                                    enf_dem,
                                                    field="field")
                          )
                          
                          s1<-aec_sub$stream %>%
                            st_transform(st_crs(proc_dem)) %>% 
                            mutate(ProvReachID=factor(ProvReachID)) %>% 
                            mutate(ProvReachID_N=as.numeric(ProvReachID))
                          
                          r1<-proc_dem
                          
                          sr1<-mask(r1,s1 %>% filter(Network_Line_Type =="Shoreline Virtual Connector") %>% st_buffer(units::set_units(15,"m")) %>% vect())
                          
                          r1[!is.na(sr1)]<-sr1-5
                          
                          sr1<-mask(r1,s1 %>% st_buffer(units::set_units(60,"m")) %>% vect())
                          
                          r1[!is.na(sr1)]<-sr1-5
                          
                          sr1<-mask(r1,s1 %>% st_buffer(units::set_units(30,"m")) %>% vect())
                          
                          r1[!is.na(sr1)]<-sr1-5
                          
                          temp_path<-tempdir()
                          
                          writeRaster(r1,file.path(temp_path,"enf_dem.tif"),overwrite=T)
                          
                          wbt_feature_preserving_smoothing(
                            dem = "enf_dem.tif",
                            output = "fill_enf_dem.tif",
                            wd =temp_path
                          )
                          
                          ihydro_out<-process_flowdir(
                            dem=file.path(temp_path,"fill_enf_dem.tif"),
                            depression_corr="fill",
                            threshold=1000L,  
                            return_products=F,
                            output_filename=file.path("data","Processed","Hydrology",paste0(aec_sub$boundary$WorkUnitName,".zip")),
                            temp_dir=NULL, 
                            verbose=T
                          )
                          
                          return(file.path("data","Processed","Hydrology",paste0(aec_sub$boundary$WorkUnitName,".zip")))
                          # ihydro_out<-generate_vectors(
                          #   input=ihydro_out,
                          #   #points=file.path(save_dir, "sites.shp"), # These are optional
                          #   #site_id_col="site_id", # Column name in points layer that corresponds to 
                          #   #                      # unique IDs that will be available in data products
                          #   #snap_distance=100L, # points that are more than 100m from closest stream are excluded
                          #   #break_on_noSnap =F, # default is to stop when any points don't snap, this will ignore that
                          #   return_products=F,
                          #   temp_dir=NULL,
                          #   verbose=F
                          # ) 
                          # 
                          # ihydro_out<-trace_flowpaths(
                          #   input=ihydro_out,
                          #   return_products=F,
                          #   temp_dir=NULL,
                          #   verbose=F
                          # )
                          
                          # ihydro_out<-ihydro::process_hydrology(
                          #   dem=file.path(temp_path,"fill_enf_dem.tif"),
                          #   output_filename=file.path("data","Processed","Hydrology",
                          #                             paste0(aec_sub$boundary$WorkUnitName,".zip")),
                          #   depression_corr = "fill",
                          #   calc_catch ="none",
                          #   pwise_dist = F,
                          #   threshold=1000L,
                          #   compress=T,
                          #   verbose=T
                          # )
                          
                          # p1<-ihydro::generate_pwisedist(
                          #   input=list(outfile=file.path("data","Processed","Hydrology",
                          #                                paste0("w01_Lake_Erie_West",".zip"))),
                          #   return_products=F,
                          #   temp_dir=NULL,
                          #   verbose=T
                          # )
                          
                          #return(ihydro_out)
                          
                        })
                      })


sub_regions_out<-map(sub_regions_out,~map(.,function(x){
  ihydro_out<-generate_vectors(
    input=list(outfile=.),
    return_products=F,
    temp_dir=NULL,
    verbose=F
  )

  ihydro_out<-trace_flowpaths(
    input=ihydro_out,
    return_products=F,
    temp_dir=NULL,
    verbose=F
  )
  
  return(ihydro_out)
}))










if (F){
  # Process subregion hydrology data
  sub_region_out<-lapply(setNames(names(sub_regions),names(sub_regions)),function(reg_nm){
    sub_all<-sub_regions[[reg_nm]]
    IH_list<-IH_regions[[reg_nm]]
    
    lapply(setNames(names(sub_all),names(sub_all)),function(sub_nm){
      temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
      if (!dir.exists(temp_path)) dir.create(temp_path)
      sub_r<-sub_all[[sub_nm]]
      
      message(paste("Process subregion hydrology:",sub_nm))
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
      
      wbt_stream_link_identifier(
        wd=temp_path,
        d8_pntr="enf_d8.tif",
        streams = "strm_grid.tif",
        output= "strm_link_ID.tif"
      )
      
      wbt_raster_streams_to_vector(
        wd=temp_path,
        d8_pntr="enf_d8.tif",
        streams = "strm_link_ID.tif",
        output= "strm_link_ID.shp"
      )
      
      wbt_strm_lines<-read_sf(file.path(temp_path,"strm_link_ID.shp"),crs=st_crs(r1)) %>% 
        rename(wbt_stream_ID=STRM_VAL)
      
      # s2<-s1 %>% 
      #   filter(Network_Line_Type !="Shoreline Virtual Connector") %>% 
      #   filter(Network_Line_Type !="Virtual Connector") %>% 
      #   head(20) %>% 
      #   group_by(ProvReachID) %>% 
      #   summarize(st_centroid(.)) %>% 
      #   #st_join(wbt_strm_lines,join=st_nearest_feature)
      #   st_join(wbt_strm_lines,join=nngeo::st_nn,k=1,maxdist=100,parallel =4)
      
      copy_files<-list(
        "strm_link_FrUSchan.tif",
        "strm_link_class.tif",
        "strm_grid.tif",
        "strm_link_ID.tif",
        "flow_accu.tif",
        "enf_d8.tif",
        "fill_enf_dem.tif"
      )
      
      #browser()
      
      copy_files<-lapply(copy_files,function(x) file.path(temp_path,x))
      
      fp<-file.path("data","Processed","Hydrology",paste0(sub_nm,".zip"))
      
      zip(fp,
          unlist(copy_files),
          flags = '-r9Xjq'
      )
      
      # 
      # if (!dir.exists(fp)) dir.create(fp)
      # 
      # for (i in copy_files){
      #   file.copy(
      #     file.path(temp_path,i),
      #     file.path(fp,i),
      #     overwrite = T
      #   )
      # }
      
      out<-c(
        sub_r,
        list(
          clean_stream_line=s1,
          whitebox_stream_line=wbt_strm_lines,
          farthest_channel_head=file.path("/vsizip",fp,"strm_link_FrUSchan.tif"),
          stream_link_class=file.path("/vsizip",fp,"strm_link_class.tif"),
          strm_grid=file.path("/vsizip",fp,"strm_grid.tif"),
          strm_link_ID=file.path("/vsizip",fp,"strm_link_ID.tif"),
          flow_accu=file.path("/vsizip",fp,"flow_accu.tif"),
          enf_d8=file.path("/vsizip",fp,"enf_d8.tif"),
          fill_enf_dem=file.path("/vsizip",fp,"fill_enf_dem.tif")
        )
      )
      
      file.remove(list.files(temp_path,full.names = T,recursive = T))
      
      return(out)
    })
  }) 
  
  #sub_region_out<-readRDS(file.path("data","Processed","Hydrology","Processed_Hydrology.rds"))
  
  # Identify Pour Points
  sub_region_out<-lapply(setNames(names(sub_region_out),names(sub_region_out)),function(reg_nm){
    sub_all<-sub_region_out[[reg_nm]]
    
    lapply(setNames(names(sub_all),names(sub_all)),function(sub_nm){
      message(paste("Identify Pour Points:",sub_nm))
      
      temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
      sub_r<-sub_all[[sub_nm]]
      
      attr<-c(
        rast(sub_r$stream_link_class),
        rast(sub_r$farthest_channel_head)
      )
      
      aec_streams_buff<-sub_r$clean_stream_line %>% 
        st_buffer(29,endCapStyle="FLAT") %>% 
        st_buffer(-7,endCapStyle="SQUARE") 
      
      
      wbt_streams_points<-sub_r$whitebox_stream_line %>% 
        st_buffer(dis=22,endCapStyle="FLAT") %>% 
        vect() %>% 
        rasterize(y=rast(sub_r$fill_enf_dem),
                  field="wbt_stream_ID") %>% 
        as.points() 
      
      wbt_streams_points2 <- wbt_streams_points %>% 
        terra::extract(attr,.) %>% 
        bind_cols(st_as_sf(wbt_streams_points),.) %>% 
        filter(strm_link_class==1 | strm_link_class==2) %>% 
        mutate(x=st_coordinates(.$geometry)[,1],
               y=st_coordinates(.$geometry)[,2]) %>% 
        group_by(wbt_stream_ID) %>% 
        st_join(aec_streams_buff,largest = TRUE) %>% 
        filter(!is.na(ProvReachID)) %>% 
        group_by(wbt_stream_ID,ProvReachID) %>% 
        mutate(x_pp=x[strm_link_FrUSchan==max(strm_link_FrUSchan)][1],
               y_pp=y[strm_link_FrUSchan==max(strm_link_FrUSchan)][1]) %>% 
        summarise(across(everything(),mean),do_union=T) %>% 
        filter(map(geometry,length)>2) %>% 
        as_tibble() %>% 
        select(wbt_stream_ID,ProvReachID,x_pp,y_pp,strm_link_class,strm_link_FrUSchan)
      
      pour_points<-sub_r$clean_stream_line %>% 
        left_join(wbt_streams_points2 %>% group_by(ProvReachID) %>% filter(strm_link_FrUSchan==max(strm_link_FrUSchan))) %>% 
        as_tibble() %>% 
        select(wbt_stream_ID,ProvReachID,x_pp,y_pp) %>% 
        filter(!is.na(wbt_stream_ID)) %>% 
        st_as_sf(coords=c("x_pp","y_pp"),crs=st_crs(sub_r$clean_stream_line),remove=F,na.fail=F)
      
      sub_r$clean_stream_line<-sub_r$clean_stream_line %>% 
        filter(ProvReachID %in% pour_points$ProvReachID)
      
      # mapview(pp1,zcol="ProvReachID")+
      #   mapview(aec_streams_buff,zcol="ProvReachID")+
      #   mapview(sub_r$whitebox_stream_line,zcol="wbt_stream_ID")
      # 
      
      out<-c(
        sub_r,
        list(
          pour_points=pour_points
        )
      )
      
      return(out)
      
    })
    
  })
  saveRDS(sub_region_out,file.path("data","Processed","Hydrology","Processed_Hydrology.rds"))
  
  
  # Generate Catchments -----------------------------------------------------
  # Is this a good idea?
  sub_region_out<-lapply(setNames(names(sub_region_out),names(sub_region_out)),function(reg_nm){
    sub_all<-sub_region_out[[reg_nm]]
    
    lapply(setNames(names(sub_all),names(sub_all)),function(sub_nm){
      message(paste("Identify Catchments:",sub_nm))
      
      temp_path<-normalizePath(file.path(td,sub_nm),winslash="\\")
      if (!dir.exists(temp_path)) dir.create(temp_path)
      sub_r<-sub_all[[sub_nm]]
      
      writeRaster(rast(sub_r$enf_d8),file.path(temp_path,"d8.tif"))
      
      write_sf(sub_r$pour_points,file.path(temp_path,"pp.shp"))
      
      wbt_unnest_basins(
        wd=temp_path,
        d8_pntr="d8.tif", 
        pour_pts="pp.shp", 
        output="catch.tiff", 
        compress_rasters =T
      )
      
      fl<-list.files(temp_path,pattern="catch",full.names = T)
      
      catch_out<-map_dfr(fl,function(x){
        r1<-rast(x)
        names(r1)<-"ProvReachID"
        
        r2<-as.polygons(r1)
        
        r2$ProvReachID<-sub_r$pour_points$ProvReachID[r2$ProvReachID]
        
        file.remove(x)
        
        return(st_as_sf(r2))
        
      })
      
      out<-c(
        sub_r,
        list(
          catchments=catch_out
        )
      )
      
      return(out)
      
    })
  })
  
  saveRDS(sub_region_out,file.path("data","Processed","Hydrology","Processed_Hydrology.rds"))
  
}

