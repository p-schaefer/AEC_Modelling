library(tidyverse)
library(sf)

booster<-"dart"
ep<-"resp_Comm_Biomass"

# Write Prediction Data frame ----------------------------------------------

pred_data_fin<-read_rds(file.path("data","final","Prediction_finaltaxa_data.rds"))
write_csv(pred_data_fin %>% 
            select(-starts_with("tx_"),-case_weight) %>% 
            distinct(),
            file.path("app","Model_Explore2","data",paste0("Predictor_data.csv")))

# Write Model Predictions -------------------------------------------------

out_final<-readRDS(file.path("data","models","LSS",paste0("Landscape_Predictions_",booster,".rds")))

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(year(as.Date(gen_SampleDate))>1994) 

preds<-out_final %>% 
  select(tx_Taxa,gen_Region,gen_ProvReachID,contains(c("resp_"))) %>% 
  left_join(
    model_data0 %>% 
      select(tx_Taxa,gen_ProvReachID,contains(c("resp_Comm"))) %>% 
      group_by(tx_Taxa,gen_ProvReachID) %>% 
      summarise(across(everything(),~median(.x,na.rm=T))) %>% 
      rename_with(.cols=contains(c("resp_Comm")),~paste0(.x,"_observed"))
  )

write_csv(preds,file.path("app","Model_Explore2","data",paste0("Model_predictions.csv")))

# Setup Region List -------------------------------------------------------
regions<-pred_data_fin$gen_Region %>% 
  as.character() %>% 
  unique() %>% 
  sort() %>% 
  .[!is.na(.)]
saveRDS(regions,file.path("app","Model_Explore2","data",paste0("regions.rds")))

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
  
  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))
sub_regions<-unlist(sub_regions,recursive = F)
saveRDS(sub_regions,file.path("app","Model_Explore2","data",paste0("AEC_Streams.rds")))


# Setup Taxa List -------------------------------------------------------
taxa<-pred_data_fin$tx_Taxa %>% 
  as.character() %>% 
  unique() %>% 
  sort() %>% 
  .[!is.na(.)]

saveRDS(taxa,file.path("app","Model_Explore2","data",paste0("taxa.rds")))


# Setup names of used predictor variables ---------------------------------

shap<-readRDS(file.path("data","models","LSS",paste0("Shap_","resp_Comm_Biomass","_",booster,".rds")))
pred_names<-colnames(shap$concentration) %>% 
  .[.!="BIAS"] %>% 
  .[!grepl("tx_",.)]
saveRDS(pred_names,file.path("app","Model_Explore2","data",paste0("pred_names.rds")))


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

write_csv(shap_out,file.path("app","Model_Explore2","data",paste0("shap.csv")))


# Save Out of sample predictions ------------------------------------------

out_res<-map_dfr(c("resp_Comm_Biomass","resp_Comm_Abundance"),#
    ~readRDS(file.path("data","models","LSS",paste0("OOB_Pred_",.x,"_",booster,".rds"))) %>% 
      select(contains(c("quant_","predicted","observed","endpoint","tx_Taxa","gen_ProvReachID")))
    )
write_csv(out_res,file.path("app","Model_Explore2","data",paste0("OOS_Pred.csv")))



