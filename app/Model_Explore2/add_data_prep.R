library(tidyverse)
library(sf)

booster<-"dart"
ep<-"resp_Comm_Biomass"

fp<-file.path("app","Model_Explore2","data",paste0("Model_datas.gpkg"))

# int_f <- function(x, ct1, rt1, gt1, ct2, rt2, gt2) {
#   #browser()
#   f1 <- gamlss.dist::dZAGA(x,ct1/rt1,1/rt1,gt1)
#   f2 <- gamlss.dist::dZAGA(x,ct2/rt1,1/rt2,gt2)
#   pmin(f1, f2)
# }
# 
# integrate(int_f, 0, 1, ct1=ct1[1], rt1=rt1[1], gt1=gt1[1], ct2=ct1[2], rt2=rt1[2], gt2=gt1[2])

# Setup Stream Lines ------------------------------------------------------
lu_master<-readRDS(file.path("data","lookups.rds"))
aec_region<-lapply(lu_master$stream_packages,function(x) file.path("data","raw","GIS",x))
names(aec_region)<-sapply(names(aec_region),function(x) file.path("data","raw","GIS",x))

td<-tempdir()
sub_regions<-lapply(names(aec_region),function(aes_nm){
  unzip(aes_nm,exdir=td)
  
  zip_cont<-list.files(file.path(td,gsub(".zip","",basename(aes_nm))),recursive = T,full.names = T)
  sub_regions<-zip_cont[grepl("/w",zip_cont) & grepl("AEC_Class.lyrx",zip_cont)]
  sub_regions<-gsub("_Class\\.lyrx","_Core\\.gdb",sub_regions)
  names(sub_regions)<-sub_regions
  
  sub_regions<-lapply(sub_regions, st_layers)
  
  sub_region_out<-map2(names(sub_regions),sub_regions,function(src,lyr) {
    list(stream=read_sf(src,lyr$name[grepl("Reach",lyr$name)]) %>% 
           select(ProvReachID,Network_Line_Type ))
  })
  
  sub_region_out2<-map2(names(sub_regions),sub_regions,function(src,lyr) {
    read_sf(src,lyr$name[grepl("Unit_Boundary",lyr$name)]) %>% pull(WorkUnitName)
  })
  
  names(sub_region_out)<-unlist(sub_region_out2)
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))

sub_regions<-map_dfr(sub_regions,.id="AEC_Region",~map_dfr(.x,.id="AEC_Region_sub",~.x$stream))
#sub_regions<-unlist(sub_regions,recursive = F)
#saveRDS(sub_regions,file.path("app","Model_Explore2","data",paste0("AEC_Streams.rds")))

sf::write_sf(sub_regions,
             fp,
             layer="AEC_Streams",
             append = F,
             delete_layer = F,
             delete_dsn = F)

con <- DBI::dbConnect(RSQLite::SQLite(), fp)

# Write Prediction Data frame ----------------------------------------------

pred_data_fin<-read_rds(file.path("data","final","Prediction_finaltaxa_data.rds"))
# write_csv(pred_data_fin %>%
#             select(-starts_with("tx_"),-case_weight) %>%
#             distinct(),
#             file.path("app","Model_Explore2","data",paste0("Predictor_data.csv")))
t1<-dplyr::copy_to(df=pred_data_fin %>%
                     select(-starts_with("tx_"),-case_weight) %>%
                     distinct(),
                   con,
                   "Predictor_Data",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)


# Write Model Predictions -------------------------------------------------

out_final<-readRDS(file.path("data","models","LSS",paste0("Landscape_Predictions_",booster,".rds")))
out_final_ref<-readRDS(file.path("data","models","LSS",paste0("Landscape_RefPredictions_",booster,".rds")))

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(year(as.Date(gen_SampleDate))>1994) 

preds<-out_final %>% 
  select(tx_Taxa,gen_Region,gen_ProvReachID,contains(c("resp_"))) %>% 
  left_join(
    out_final_ref %>% 
      select(tx_Taxa,gen_Region,gen_ProvReachID,contains(c("resp_"))) %>% 
      rename_with(.cols=contains("resp_"),~paste0(.x,"_ref"))
  ) %>% 
  mutate(
    resp_Comm_Abundance_predicted_refdiff=resp_Comm_Abundance_predicted_ref-resp_Comm_Abundance_predicted,
    resp_Comm_Abundance_quant_0.95_refdiff=resp_Comm_Abundance_quant_0.95_ref-resp_Comm_Abundance_quant_0.95,
    resp_Comm_Abundance_quant_0.75_refdiff=resp_Comm_Abundance_quant_0.75_ref-resp_Comm_Abundance_quant_0.75,
    resp_Comm_Abundance_quant_0.66_refdiff=resp_Comm_Abundance_quant_0.66_ref-resp_Comm_Abundance_quant_0.66,
    resp_Comm_Abundance_quant_0.5_refdiff=resp_Comm_Abundance_quant_0.5_ref-resp_Comm_Abundance_quant_0.5,
    resp_Comm_Abundance_quant_0.33_refdiff=resp_Comm_Abundance_quant_0.33_ref-resp_Comm_Abundance_quant_0.33,
    resp_Comm_Abundance_quant_0.25_refdiff=resp_Comm_Abundance_quant_0.25_ref-resp_Comm_Abundance_quant_0.25,
    resp_Comm_Abundance_quant_0.05_refdiff=resp_Comm_Abundance_quant_0.05_ref-resp_Comm_Abundance_quant_0.05,
    resp_Comm_Biomass_predicted_refdiff=resp_Comm_Biomass_predicted_ref-resp_Comm_Biomass_predicted,
    resp_Comm_Biomass_quant_0.95_refdiff=resp_Comm_Biomass_quant_0.95_ref-resp_Comm_Biomass_quant_0.95,
    resp_Comm_Biomass_quant_0.75_refdiff=resp_Comm_Biomass_quant_0.75_ref-resp_Comm_Biomass_quant_0.75,
    resp_Comm_Biomass_quant_0.66_refdiff=resp_Comm_Biomass_quant_0.66_ref-resp_Comm_Biomass_quant_0.66,
    resp_Comm_Biomass_quant_0.5_refdiff=resp_Comm_Biomass_quant_0.5_ref-resp_Comm_Biomass_quant_0.5,
    resp_Comm_Biomass_quant_0.33_refdiff=resp_Comm_Biomass_quant_0.33_ref-resp_Comm_Biomass_quant_0.33,
    resp_Comm_Biomass_quant_0.25_refdiff=resp_Comm_Biomass_quant_0.25_ref-resp_Comm_Biomass_quant_0.25,
    resp_Comm_Biomass_quant_0.05_refdiff=resp_Comm_Biomass_quant_0.05_ref-resp_Comm_Biomass_quant_0.05,
  ) %>% 
  left_join(
    model_data0 %>% 
      select(tx_Taxa,gen_ProvReachID,contains(c("resp_Comm"))) %>% 
      group_by(tx_Taxa,gen_ProvReachID) %>% 
      summarise(across(everything(),~median(.x,na.rm=T))) %>% 
      rename_with(.cols=contains(c("resp_Comm")),~paste0(.x,"_observed"))
  )

t1<-dplyr::copy_to(df=preds,
                   con,
                   "Model_Predictions",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

# Setup Region List -------------------------------------------------------
regions<-pred_data_fin$gen_Region %>% 
  as.character() %>% 
  unique() %>% 
  sort() %>% 
  .[!is.na(.)]

t1<-dplyr::copy_to(df=tibble(regions=regions),
                   con,
                   "Region_names",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

# saveRDS(regions,file.path("app","Model_Explore2","data",paste0("regions.rds")))

# Setup Taxa List -------------------------------------------------------
taxa<-pred_data_fin$tx_Taxa %>% 
  as.character() %>% 
  unique() %>% 
  sort() %>% 
  .[!is.na(.)]

t1<-dplyr::copy_to(df=tibble(taxa=taxa),
                   con,
                   "Taxa_names",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)
# saveRDS(taxa,file.path("app","Model_Explore2","data",paste0("taxa.rds")))

# Setup names of used predictor variables ---------------------------------

shap<-readRDS(file.path("data","models","LSS",paste0("Shap_","resp_Comm_Biomass","_",booster,".rds")))
pred_names<-colnames(shap$concentration) %>% 
  .[.!="BIAS"] %>% 
  .[!grepl("tx_",.)]
#saveRDS(pred_names,file.path("app","Model_Explore2","data",paste0("pred_names.rds")))

t1<-dplyr::copy_to(df=tibble(pred_names=pred_names),
                   con,
                   "Predictor_names",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)
# Save Shap Values --------------------------------------------------------

shap_out<-map_dfr(c("resp_Comm_Biomass","resp_Comm_Abundance"), #
        function(x){
          shap<-readRDS(file.path("data","models","LSS",paste0("Shap_",x,"_",booster,".rds")))
          
          out<-list(
            Mean=shap$concentration*shap$rate,
            `Presence/Absence`=shap$gate
          )
          
          out$Mean[is.infinite(out$Mean)]<-NA_real_
          
          map2_dfr(out,names(out),
                   ~mutate(as.data.frame(.x),shape_param=.y) %>%
                     select(-BIAS) %>% 
                     mutate(endpoint=x) %>% 
                     bind_cols(
                       shap[["raw_data"]] %>% 
                         rename_with(~paste0("sel_",.x))
                     )
                   )
        })

shap_out<-shap_out %>% 
  select(-sel_case_weight)

#write_csv(shap_out,file.path("app","Model_Explore2","data",paste0("shap.csv")))
t1<-dplyr::copy_to(df=shap_out,
                   con,
                   "SHAP_scores",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

# Save Out of sample predictions ------------------------------------------

out_res<-map_dfr(c("resp_Comm_Biomass","resp_Comm_Abundance"),#
    ~readRDS(file.path("data","models","LSS",paste0("OOB_Pred_",.x,"_",booster,".rds"))) %>% 
      select(contains(c("quant_","predicted","observed","endpoint","tx_Taxa","gen_ProvReachID")))
    )
#write_csv(out_res,file.path("app","Model_Explore2","data",paste0("OOS_Pred.csv")))
t1<-dplyr::copy_to(df=out_res,
                   con,
                   "OOS_Predictions",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

DBI::dbDisconnect(con)
