library(tidyverse)
library(tidymodels)

tx_data<-read_csv(file.path("data","final","Model_building_rawtaxa_data.csv"))

adj<-0.005

inv.logit <- function(f,a) {
  a <- (1-2*a)
  zapsmall((a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f))))
}

# Finalize Model Data -----------------------------------------------------

model_data<-tx_data %>% 
  filter(!is.na(`name to use`)) %>% 
  filter(!is.na(ProvReachID)) %>% 
  mutate(
    tx_Repro_1=str_split(`Reproductive Guild`,": ",simplify = T)[,1],
    tx_Repro_2=str_split(`Reproductive Guild`,": ",simplify = T)[,2],
    tx_Repro_3=str_split(`Reproductive Guild`,": ",simplify = T)[,3],
  ) %>% 
  # mutate(
  #   Lake_Influence_Code=as.numeric(as.factor(Lake_Influence_Code)),
  # ) %>% 
  select(# Taxa Info
    gen_ProvReachID=ProvReachID,
    gen_link_id=link_id,
    gen_SampleEventID=SampleEventID,
    gen_StreamName=StreamName,
    gen_SampleDate=SampleDate,
    # Responses
    resp_Comm_Biomass=Comm_Biomass,
    resp_Comm_Abundance=Comm_Abundance,
    resp_Perc_Biomass=Perc_Biomass,
    resp_Perc_Abundance=Perc_Abundance,
    # Taxa Data
    tx_Taxa=`name to use`,
    tx_Family=Family,
    tx_Tolerance=Tolerance,
    tx_Trophic_Class=`Trophic Class`,
    tx_Thermal_Regime=`Thermal Regime`,
    tx_General_Habitat=`General Habitat(s)`,
    tx_Environment=Environment,
    tx_Repro_1,
    tx_Repro_2,
    tx_Repro_3,
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
  group_by(gen_ProvReachID) %>% 
  mutate(case_weight=importance_weights(1/n())) %>% 
  ungroup() %>% 
  select(case_weight,everything()) %>% 
  mutate(
    resp_Comm_Biomass=log1p(resp_Comm_Biomass),
    resp_Comm_Abundance=log1p(resp_Comm_Abundance),
    resp_Perc_Biomass=car::logit(resp_Perc_Biomass,F,adj),
    resp_Perc_Abundance=car::logit(resp_Perc_Abundance,F,adj)
  ) 

taxa_prop <- model_data %>% 
  group_by(tx_Taxa) %>% 
  summarise(
    mean_biomass_perc=inv.logit(mean(resp_Perc_Biomass,na.rm=T),adj)*100,
    mean_abund_perc=inv.logit(mean(resp_Perc_Abundance,na.rm=T),adj)*100
  ) %>% 
  arrange(desc(mean_abund_perc)) %>% 
  filter(mean_abund_perc>0.10)

model_data<-model_data %>% 
  filter(tx_Taxa %in% taxa_prop$tx_Taxa) %>% 
  mutate(across(where(is.character),~factor(.))) 

write_rds(model_data,file.path("data","final","Model_building_finaltaxa_data.rds"))
