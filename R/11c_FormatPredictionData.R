library(tidyverse)
library(tidymodels)
library(furrr)
library(sf)

lu_master<-readRDS(file.path("data","lookups.rds"))

plan(multisession(workers=8))

fl<-list.files(file.path("data","Processed","ihydro"),full.names = T)
fl_base<-fl[!grepl("_loi|_DW|.csv|.ini|old",fl)]
names(fl_base)<-gsub(".gpkg","",basename(fl_base))

all_lc<-read_csv(file.path("data","final","All_LC_Attr.csv")) %>% 
  select(-`...1`,-status)

int_data<-read_rds(file.path("data","final","int_results.rds"))
int_data_out<-map2(int_data,
                   names(int_data),
                   ~.x$aec_steam_out %>% 
                     dplyr::mutate(lon = sf::st_coordinates(sf::st_centroid(.))[,1],
                                   lat = sf::st_coordinates(sf::st_centroid(.))[,2]) %>% 
                     as_tibble() %>% 
                     select(-Shape) %>% 
                     left_join(all_lc %>% 
                                 filter(grepl(gsub("_AEC_Core","",.y),region)),
                               by="link_id") %>% 
                     filter(!is.na(region))) %>% 
  bind_rows()

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds"))

#mapview::mapview(int_data_out$w01_AEC_Core,zcol="br_Urban_HAiFLS_prop")
# Finalize Model Data -----------------------------------------------------

pred_data<-int_data_out %>% 
  select(# Taxa Info
    gen_Region=region,
    gen_ProvReachID=ProvReachID,
    gen_link_id=link_id,
    # Habitat
    hb_Temperature_Class=Temperature_Class,
    hb_Temperature=Temperature_30yr_MeanJuly,
    hb_GDDair_UpstreamCatchmentMean=GDDair_UpstreamCatchmentMean,
    hb_Turbidity=Turbidity_percUpstreamChannel_TurbGeo,
    hb_Slope=Slope_ReachChannel_Percent,
    hb_BFI_RCA=BFI_RCA,
    hb_BFI_UCA=BFI_UCA,
    hb_UCA=Upstream_Catchment_Area,
    hb_Lake_Inf=Lake_Influence_Code,
    hb_Wadeability=Wadeability,
    hb_lat=lat,
    hb_lon=lon,
    # Stressor
    starts_with("nr_"),
    starts_with("br_"),
    starts_with("LDI_")
  ) %>% 
  mutate(case_weight=importance_weights(1)) %>% 
  select(gen_Region,any_of(colnames(model_data0))) 

addn_tx<-model_data0 %>% 
  select(starts_with("tx_")) %>% 
  distinct()

pred_data_fin<-pred_data %>% 
  mutate(tx=list(addn_tx)) %>% 
  unnest(tx)%>% 
  mutate(cat_resp_Comm_Biomass=factor("0-1"),
         cat_resp_Comm_Abundance=factor("0-1"))%>% 
  mutate(across(where(is.character),~factor(.))) 

write_rds(pred_data_fin,file.path("data","final","Prediction_finaltaxa_data.rds"))

