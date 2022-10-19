library(sf)
library(sfnetworks)
library(tidyverse)

# This script is still in development, need to figure out 
par.dir<-getwd()

td<-tempdir()
lu_master<-readRDS(file.path("data","lookups.rds"))

hydrology<-readRDS(file.path("data","Processed","Hydrology","Processed_Hydrology.rds"))

streams<-map(hydrology,~map_dfr(.,~filter(.$clean_stream_line, 
                                          ProvReachID %in% .$pour_points$ProvReachID,
                                          Network_Line_Type !="Shoreline Virtual Connector"))) 



data_loc<-file.path(normalizePath("../.."),"Data")
HW_loc<-file.path(data_loc,"HydroPrep")
pnt_loc<-file.path(data_loc,"SmplLoc_Points")
gis_loc<-file.path(data_loc,"GIS_Preds")

site_path_pool<-file.path(pnt_loc, "Final_Centroids.shp")

stream_lines<-file.path(HW_loc, "strm_lines.shp")

t1<-read_sf(stream_lines)

ln_sfnetwork <- as_sfnetwork(t1)

# To add prediction points:
if (T){
  
  pred_sites<-read_sf(file.path(pnt_loc,"pred_points.shp"))
  data_sites<-read_sf(site_path_pool)
  
  all_sites<-data_sites %>% 
    dplyr::select(UUID,geometry) %>% 
    bind_rows(
      pred_sites %>% 
        dplyr::select(UUID,geometry)
    ) %>% 
    distinct()
  
  ln_sfnetwork <- st_network_blend(ln_sfnetwork, all_sites)
  
} else {
  ln_sfnetwork <- st_network_blend(ln_sfnetwork, read_sf(site_path_pool))
  
}

ud_ln_sfnetwork<-as_sfnetwork(ln_sfnetwork,directed=F)

edges <- ln_sfnetwork %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) %>% 
  mutate(FD = 1:n()) %>% 
  st_as_sf()

nodes<-ln_sfnetwork %>% 
  activate("nodes") %>% 
  mutate(FD = 1:n()) %>% 
  st_as_sf() %>% 
  filter(!is.na(UUID))

stn_nms<-setNames(nodes$UUID,nodes$UUID)

# NONE OF THE BELOW WILL WORK RIGHT NOW, NEEDS TO BE FIXED

library(future.apply)

plan(list(tweak(multisession, workers = 2)), tweak(multisession, workers = 4))
#plan(multisession)

# stn_nms=list("201043_1",
#              "200087_1",
#              "199765_1",
#              "199654_1")
# names(stn_nms)<-stn_nms

pairwise_dists<-future_lapply(stn_nms[!grepl("PRED",stn_nms)],function(x){
  future_lapply(stn_nms,function(y) {
    st_network_paths(ln_sfnetwork,
                     from = nodes[nodes$UUID==x,] %>% pull(FD),
                     to =  nodes[nodes$UUID==y,] %>% pull(FD),
                     weights=NA) %>% 
      dplyr::select(edge_paths) %>% 
      mutate(source=x, 
             target=y) %>% 
      mutate(directed_path_length=map_dbl(edge_paths,function(x){
        if (length(x)==0) return(0)
        edges[x,] %>%
          pull(weight) %>%
          sum(./10000)
      })) %>% 
      dplyr::select(source:directed_path_length) %>% 
      bind_cols(
        st_network_paths(ud_ln_sfnetwork,
                         from = nodes[nodes$UUID==x,] %>% pull(FD),
                         to =  nodes[nodes$UUID==y,] %>% pull(FD),
                         weights=NA) %>% 
          dplyr::select(edge_paths) %>% 
          mutate(undirected_path_length=map_dbl(edge_paths,function(x){
            if (length(x)==0) return(0)
            edges[x,] %>%
              pull(weight) %>%
              sum(./10000)
          })) %>% 
          dplyr::select(undirected_path_length)
      )
    
  }) %>% 
    bind_rows()
})%>% 
  bind_rows()

write_rds(pairwise_dists,file.path(pnt_loc,"pairwise_dists.rds"))

pairwise_dists<-readRDS(file.path(pnt_loc,"pairwise_dists.rds"))

gis_data<-readRDS(file.path(pnt_loc,"tg_O_multi_catchment.rds")) %>% 
  bind_rows(readRDS(file.path(pnt_loc,"pred_catchment.rds"))) %>% 
  mutate(Area=st_area(geometry)) %>% 
  as_tibble() %>% 
  dplyr::select(UUID,Area) %>% 
  distinct()

pairwise_dists2 <- pairwise_dists %>%
  rowwise() %>%
  mutate(prop_shared_catchment=case_when(
    directed_path_length>0 ~ gis_data$Area[gis_data$UUID==source]/gis_data$Area[gis_data$UUID==target],
    T ~ 0
  )) %>%
  ungroup()

write_rds(pairwise_dists2,file.path(pnt_loc,"pairwise_dists.rds"))
