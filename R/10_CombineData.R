library(tidyverse)
library(sf)
library(future.apply)

HW_save_loc<-file.path("data","HW")
if (!dir.exists(HW_save_loc)) dir.create(HW_save_loc)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

n_cores<-availableCores(logical = F)-1

plan(multisession(workers=8))


# Bio Data ----------------------------------------------------------------

bio_dt<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Results - Wide")
bio_pnt<- bio_dt %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>%
  #filter(SampleDate>=as.Date("1990-01-01")) %>% 
  select(StreamName:SampleEventID) %>% 
  distinct() %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=st_crs(4326),remove = T) %>% 
  st_transform(st_crs(3161))

# AEC region processing ---------------------------------------------------

aec_region<-lapply(lu_master$stream_packages,function(x) file.path("data","raw","GIS",x))
names(aec_region)<-sapply(names(aec_region),function(x) file.path("data","raw","GIS",x))

fl<-list.files(file.path("data","Processed","ihydro"),".gpkg",full.names = T)
fl<-fl[!grepl("_DW.gpkg",fl)]

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


# Identify Pour Points
#plan(multisession(workers=8))
sub_region_out<-lapply(sub_regions,function(sub_regions_1){
  
  lapply(sub_regions_1,function(sub_r){
    browser()
    ihydro_r<-fl[grepl(sub_r$boundary$WorkUnitName,fl)]
    
    con <- DBI::dbConnect(RSQLite::SQLite(), ihydro_r)
    
    attr<-tbl(con,"stream_links_attr") %>% 
      collect() 
    
    attr_all<-tbl(con,"stream_points_attr") %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    ihydro_streams<-read_sf(ihydro_r,"stream_lines")
    ihydro_points<-read_sf(ihydro_r,"stream_points")
    ihydro_links<-read_sf(ihydro_r,"stream_links")
    

    # Adds a buffer around the stream network
    aec_streams_buff<-sub_r$stream %>% 
      filter(!Network_Line_Type %in% c("Shoreline Virtual Connector","Virtual Connector")) %>% 
      st_buffer(29,endCapStyle="FLAT") %>% 
      st_buffer(-3,endCapStyle="SQUARE") 
    
    
    t_out<-ihydro_links %>% 
      left_join(attr %>% select(link_id,trib_id,USChnLn_Fr), by = "link_id") %>% 
      st_buffer(dis=22) %>% 
      st_join(aec_streams_buff %>% 
                select(ProvReachID,
                       Network_Line_Type,
                       Temperature_30yr_MeanJuly,
                       Turbidity_percUpstreamChannel_TurbGeo,
                       Slope_ReachChannel_Percent,
                       BFI_RCA,
                       BFI_UCA,
                       GDDair_UpstreamCatchmentMean,
                       Upstream_Catchment_Area,
                       Lake_Influence_Code,
                       Wadeability
                )
              ,largest = TRUE) %>% 
      mutate(ProvReachID_fin=ProvReachID) %>% 
      group_by(ProvReachID) %>% 
      mutate(ProvReachID_fin=case_when(
        trib_id==trib_id[USChnLn_Fr==max(USChnLn_Fr)] ~ ProvReachID_fin,
        T ~ NA_character_
      )) %>% 
      ungroup() %>% 
      mutate(ProvReachID=ProvReachID_fin) %>% 
      select(-ProvReachID_fin) %>% 
      as_tibble() %>% 
      select(-geom) %>% 
      group_by(trib_id) %>% 
      arrange(desc(USChnLn_Fr)) %>% 
      fill(ProvReachID,.direction="down") %>% 
      ungroup() %>% 
      arrange(link_id)
    
    bio_pnt_sub<-bio_pnt  %>% 
      st_join(ihydro_points %>%
                st_buffer(dis=22) %>% 
                st_join(aec_streams_buff %>%
                          filter(!Network_Line_Type %in% c("Shoreline Virtual Connector","Virtual Connector")) %>% 
                          select(Network_Line_Type),
                        largest = TRUE) %>% 
                filter(!is.na(Network_Line_Type)) %>% 
                select(-Network_Line_Type) %>% 
                left_join(attr_all %>%
                            select(ID,link_id),
                          by="ID"),
              join =nngeo::st_nn,k=1,maxdist=60,parallel =8) 
    
    bio_pnt_sub2<-bio_pnt_sub %>% 
      filter(!is.na(link_id)) %>% 
      mutate(WorkUnitName=sub_r$boundary$WorkUnitName)
    
    # mapview::mapview(t_out,zcol="ProvReachID")+
    #   mapview::mapview(ihydro_links)+
    #   mapview::mapview(ihydro_streams)+
    #   mapview::mapview(aec_streams_buff,zcol="ProvReachID")+
    #   mapview::mapview(bio_pnt_sub2)
    
    out<-
      list(
        attr_linkID=t_out,
        joined_biopnt=bio_pnt_sub2
      )
    
    return(out)
    
  })
  
})

plan(sequential)

sub_region_out2<-unlist(sub_region_out,recursive=F)

saveRDS(sub_region_out2,file.path("data","final","int_results.rds"))
