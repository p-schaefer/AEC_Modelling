library(tidyverse)
source("R/00_Functions/Endpoint_Calc_Functions.R")

fp<-file.path("app","Model_Explore2","data",paste0("Model_data_v2.gpkg"))

file.remove(fp)
file.copy(file.path("app","Model_Explore2","data",paste0("Model_data_v2_backup.gpkg")),fp)

con <- DBI::dbConnect(RSQLite::SQLite(), fp)

pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)

# Read in Lookups ---------------------------------------------------------

lookup_tbl<-read.csv(file.path("data","fish_lookup.csv")) %>% 
  mutate(name.to.use=trimws(name.to.use))
colnames(lookup_tbl)[colnames(lookup_tbl)=="OMNR.Code"]<-"SpeciesCode"

SATI_df<-read.csv(file.path("data","raw","Bio","lkpSpeciesHabitatRequirement.csv")) %>% 
  select(SpeciesCode=FWISFishCode,OPT_TEMP) %>% 
  distinct() %>% 
  filter(!is.na(OPT_TEMP))

lookup_tbl<-left_join(lookup_tbl,SATI_df,by="SpeciesCode")

info_cols<-c("Thermal.Regime","Trophic.Class","Tolerance","SpeciesCode","Environment","Reproductive.Code","Reproductive.Guild","OPT_TEMP")

lookup_tbl<-lookup_tbl %>% 
  select(tx_Taxa=name.to.use,all_of(info_cols)) %>% 
  group_by(tx_Taxa) %>% 
  summarize(across(everything(),~head(.x,1))) %>% 
  distinct()


# Setup Endpoints ---------------------------------------------------------
sel_ep<-c(`Community Total Biomass`="Comm_Biomass",
          `Community Total Density`="Comm_Abundance",
          `Community Total Richness`="Comm_Richness",
          `SATI Biomass`="Therm_SATI_Bioperc",
          `SATI Density`="Therm_SATI_Abuperc",
          `% Cold Water Stenotherm % Biomass`="Therm_cold_Bioperc",
          #`Cool Water Stenotherm % Biomass`="Therm_cool_Bioperc",
          #`Cool+Cold Water Stenotherm % Biomass`="Therm_coolcold_Bioperc",
          `% Cold Water Stenotherm % Density`="Therm_cold_Abuperc",
          #`Cool Water Stenotherm % Density`="Therm_cool_Abuperc",
          #`Cool+Cold Water Stenotherm % Density`="Therm_coolcold_Abuperc",
          `% Tolerant Taxa % Biomass`="Tol_tol_Bioperc",
          `% Intolerant Taxa % Biomass`="Tol_intol_Bioperc",
          `% Tolerant Taxa % Density`="Tol_tol_Abuperc",
          `% Intolerant Taxa % Density`="Tol_intol_Abuperc",
          `Tolerant Taxa Richness`="Tol_tol_Rich",
          `Intolerant Taxa Richness`="Tol_intol_Rich"
) %>% 
  enframe(name="name",value="Endpoint") %>% 
  mutate(Endpoint_type=case_when(
    grepl("_Bioperc$",Endpoint) ~ "Biomass",
    grepl("_Abuperc$",Endpoint) ~ "Abundance",
    grepl("_Bio$",Endpoint) ~ "Biomass",
    grepl("_Abu$",Endpoint) ~ "Abundance",
    grepl("_Rich$",Endpoint) ~ "Biomass",
    Endpoint=="Comm_Biomass" ~ "Biomass",
    Endpoint=="Comm_Abundance" ~ "Abundance",
    Endpoint=="Comm_Richness" ~ "Biomass"
  )) %>% 
  mutate(Endpoint_group=gsub("_Biomass|_Abundance|_Richness|_Bioperc|_Abuperc|_Rich","",Endpoint)) %>% 
  mutate(Endpoint_group2=gsub(" % Density","",name,fixed = T)) %>% 
  mutate(Endpoint_group2=gsub(" % Biomass","",Endpoint_group2,fixed = T)) %>% 
  mutate(Endpoint_group2=gsub(" Density","",Endpoint_group2,fixed = T)) %>% 
  mutate(Endpoint_group2=gsub(" Biomass","",Endpoint_group2,fixed = T))

# Setup Taxa List -------------------------------------------------------
calc_ep<-sel_ep$Endpoint_group2 %>% 
  as.character() %>% 
  unique() %>% 
  #sort() %>% 
  .[!is.na(.)]

t1<-dplyr::copy_to(df=tibble(calc_ep=calc_ep),
                   con,
                   "CalcEP_names",
                   overwrite =T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)
# Setup Prediction list ---------------------------------------------------

Model_Predictions_list<-list(
  Predicted=c(EstimatedBiomass="resp_Comm_Biomass_quant_0.75",NumberOfFish="resp_Comm_Abundance_quant_0.75"),
  Reference=c(EstimatedBiomass="resp_Comm_Biomass_quant_0.75_ref",NumberOfFish="resp_Comm_Abundance_quant_0.75_ref"),
  Observed=c(EstimatedBiomass="resp_Comm_Biomass_observed",NumberOfFish="resp_Comm_Abundance_observed")
)
Model_Predictions_list_sub<-unlist(Model_Predictions_list)
names(Model_Predictions_list_sub)<-NULL



sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
  select(tx_Taxa,gen_Region,gen_ProvReachID,
         all_of(Model_Predictions_list_sub)
         ) %>% 
  collect()

region<-sel_modelpredictions %>% 
  select(gen_Region,gen_ProvReachID) %>% 
  distinct()

sel_modelpredictions <- sel_modelpredictions %>% 
  left_join(lookup_tbl,by="tx_Taxa")


out<-map(Model_Predictions_list, 
    function(x){
      sel_modelpredictions %>% 
        select(ProvReachID=gen_ProvReachID,all_of(setNames(x,NULL)),all_of(info_cols)) %>% 
        mutate(across(any_of(setNames(x,NULL)),~expm1(.x))) %>% 
        filter(!if_any(any_of(setNames(x,NULL)),~is.na(.x))) %>% 
        filter(!if_all(any_of(setNames(x,NULL)),~.x==0)) %>% 
        set_names(c("SampleEventID",names(x),info_cols)) %>% 
        mutate(SampleDate="2000-01-01") %>% 
        calc_fish_ep() %>% 
        select(SampleEventID,SampleDate,all_of(setNames(sel_ep$Endpoint,NULL))) %>% 
        pivot_longer(c(everything(),-SampleEventID,-SampleDate),names_to="Endpoint",values_to="val") %>% 
        left_join(sel_ep,by=c("Endpoint"))  %>% 
        select(gen_ProvReachID=SampleEventID,tx_Taxa=Endpoint_group2,Endpoint_type,val) %>% 
        pivot_wider(names_from="Endpoint_type",values_from="val") %>% 
        mutate(Abundance=if_else(is.na(Abundance),Biomass,Abundance)) %>% 
        set_names(c("gen_ProvReachID","tx_Taxa",x)) %>% 
        mutate(across(any_of(setNames(x,NULL)),
                      ~case_when(
                        tx_Taxa %in% c("Community Total") ~ log1p(.x),
                        grepl("%",tx_Taxa) ~ .x*100,
                        T ~ .x
                      )))
    }
) %>% 
  purrr::reduce(left_join,by=c("gen_ProvReachID","tx_Taxa")) %>% 
  mutate(
    resp_Comm_Abundance_quant_0.75_refdiff=resp_Comm_Abundance_quant_0.75-resp_Comm_Abundance_quant_0.75_ref,
    resp_Comm_Biomass_quant_0.75_refdiff=resp_Comm_Biomass_quant_0.75-resp_Comm_Biomass_quant_0.75_ref,
  )

out<-left_join(out,region,by="gen_ProvReachID")
 
t1<-dplyr::copy_to(df=out,
                   con,
                   "Model_Predictions",
                   overwrite =F,
                   append=T,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

sel_modelOOSpredictions<-tbl(con,"OOS_Predictions") %>% 
  collect()


s6<-RSQLite::dbSendQuery(con, "pragma vacuum;")
s7<-RSQLite::dbSendQuery(con, "pragma optimize;")


DBI::dbDisconnect(con)
