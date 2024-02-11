library(tidyverse)
library(sf)
library(future.apply)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

n_cores<-availableCores(logical = F)-1

plan(multisession(workers=2))


# Bio Data ----------------------------------------------------------------

bio_dt<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Results - Wide")
bio_pnt<- bio_dt %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>%
  #filter(SampleDate>=as.Date("1990-01-01")) %>% 
  select(StreamName:SampleEventID) %>% 
  distinct() %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=st_crs(4326),remove = T) %>% 
  st_transform(st_crs(3161))


# bio_pnt<-bio_pnt %>% 
#   filter(SampleEventID %in% readRDS("missing.rds"))

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
sub_region_out<-future_lapply(sub_regions,function(sub_regions_1){
  
  lapply(sub_regions_1,function(sub_r){
    #browser()
    ihydro_r<-fl[grepl(sub_r$boundary$WorkUnitName,fl)]
    ihydro_r<-ihydro_r[!grepl("loi",ihydro_r)]
    
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
      st_buffer(60,endCapStyle="FLAT") %>% 
      st_buffer(-45,endCapStyle="SQUARE") %>% 
      select(ProvReachID)
    
    ihydro_streams_buff<-ihydro_streams %>% 
      st_buffer(60,endCapStyle="FLAT") %>% 
      st_buffer(-45,endCapStyle="SQUARE") %>% 
      select(link_id)
    
    streams_join1<- aec_streams_buff %>% 
      st_join(ihydro_streams_buff,
              largest = TRUE) %>% 
      as_tibble() %>% 
      select(link_id,ProvReachID)
    
    streams_join2<- ihydro_streams_buff %>% 
      st_join(aec_streams_buff,
              largest = TRUE) %>% 
      as_tibble() %>% 
      select(link_id,ProvReachID) %>% 
      left_join(attr %>% select(link_id,USChnLn_Fr) %>% mutate(link_id=as.numeric(link_id)))
    
    stream_joins_final<-full_join(streams_join1,streams_join2) %>%
      filter(!is.na(link_id)) %>%
      filter(!is.na(ProvReachID)) %>% 
      group_by(ProvReachID) %>% 
      summarize(link_id=unique(link_id[USChnLn_Fr==max(USChnLn_Fr)]))
    
    #stream_joins_final<-streams_join1
    
    aec_steam_out<-sub_r$stream %>% 
      left_join(stream_joins_final) %>% 
      select(ProvReachID,link_id,everything())
    
    ihydro_streams_out<-ihydro_streams %>% 
      left_join(stream_joins_final) %>% 
      select(ProvReachID,link_id,everything())
    
    bio_pnt_out <- bio_pnt %>% 
      st_crop(sub_r$boundary) %>% 
      filter(st_within(.,sub_r$boundary,sparse = F)[,1])
    
    if (nrow(bio_pnt_out)>0){
      bio_pnt_out<-bio_pnt_out %>% 
        st_join(sub_r$stream %>% 
                  filter(!Network_Line_Type %in% c("Shoreline Virtual Connector","Virtual Connector")),
                join =nngeo::st_nn,k=1,maxdist=500,parallel =1) %>% 
        filter(!is.na(ProvReachID)) %>% 
        left_join(stream_joins_final,by="ProvReachID")
    }

    out<-
      list(
        aec_steam_out=aec_steam_out,
        ihydro_streams_out=ihydro_streams_out,
        bio_pnt_out=bio_pnt_out
      )
    
    return(out)
    
  })
  
})

plan(sequential)

sub_region_out2<-unlist(sub_region_out,recursive=F)

saveRDS(sub_region_out2,file.path("data","final","int_results.rds"))



